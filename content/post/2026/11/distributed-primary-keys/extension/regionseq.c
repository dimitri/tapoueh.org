/*-------------------------------------------------------------------------
 *
 * regionseq.c
 *	  A sequence access method for ids that several regions mint at once.
 *
 * An id is 63 bits, most significant first:
 *
 *	  41 bits	milliseconds since 2026-01-01 00:00:00 UTC
 *	   6 bits	region number, from the regionseq.region_id setting
 *	   4 bits	timeline of the server, modulo 16
 *	  12 bits	counter, within one millisecond of one region
 *
 * The timeline changes at every promotion, and when a server is restored to a
 * point in time.  A promoted standby may have a stale copy of the state and a
 * clock that is behind the old primary's; what it makes is still different from
 * what the old primary made, because the timeline bits are.  Sixteen promotions of
 * one region within the time a clock was wrong are needed to repeat an id; two
 * standbys promoted independently from the same primary (split brain) get the
 * same timeline, and are a matter for fencing.
 *
 * 4,096 ids per millisecond, as Snowflake has, is 4.1 million a second, about
 * what one sequence can do under its lock; a region that makes more takes the
 * next millisecond and its ids run ahead of the clock for a while.
 *
 * Ids of one region increase strictly, whatever the clock does: the sequence
 * keeps the last (millisecond, counter) it handed out and never goes below
 * it.
 *
 *	  - the clock is ahead: the id gets the clock's millisecond, counter 0;
 *	  - the clock is behind or equal (same millisecond, or the clock was set
 *		back): the id keeps the last millisecond and adds 1 to the counter;
 *	  - the counter would pass 4095: the id takes the next millisecond
 *		instead of waiting for it.
 *
 * The state is also kept safe across a crash and a promotion, the way a
 * plain sequence does it with its 32 values: a high-water mark in
 * milliseconds is logged to the WAL ahead of the ids (LEAD_MS), and what the
 * WAL holds is "everything below the high-water mark is used".  One WAL
 * record a second at most, however many ids are made.
 *
 * Portions Copyright (c) 1996-2026, PostgreSQL Global Development Group
 *
 * IDENTIFICATION
 *	  contrib/regionseq/regionseq.c
 *
 *-------------------------------------------------------------------------
 */

#include "postgres.h"

#include "access/generic_xlog.h"
#include "access/htup_details.h"
#include "access/sequenceam.h"
#include "access/sequence_page.h"
#include "access/xact.h"
#include "access/xlog.h"
#include "commands/tablecmds.h"
#include "fmgr.h"
#include "miscadmin.h"
#include "nodes/makefuncs.h"
#include "storage/bufmgr.h"
#include "utils/guc.h"
#include "utils/timestamp.h"

PG_MODULE_MAGIC;

/* "special area" of the sequence's page */
#define REGIONSEQ_MAGIC		0x5251

typedef struct regionseq_magic
{
	uint32		magic;
} regionseq_magic;

#define REGIONSEQ_EPOCH_MS		INT64CONST(1767225600000)	/* 2026-01-01 UTC */
#define REGIONSEQ_TIME_SHIFT	22
#define REGIONSEQ_REGION_SHIFT	16
#define REGIONSEQ_TIMELINE_SHIFT	12
#define REGIONSEQ_TIMELINE_MASK	0xF
#define REGIONSEQ_COUNTER_MASK	0xFFF
#define REGIONSEQ_REGION_MASK	0x3F
#define REGIONSEQ_TIME_MASK		((INT64CONST(1) << 41) - 1)

/* how far ahead of the ids the WAL holds the state, in milliseconds */
#define REGIONSEQ_LEAD_MS		1000

/*
 * The tuple of a regionseq sequence.  Same layout as the columns that
 * regionseq_init() adds, in the order of attalign.
 */
typedef struct FormData_regionseq_data
{
	int64		last_ms;		/* milliseconds of the last id */
	int32		counter;		/* counter of the last id */
	bool		is_called;
	int64		high_water;		/* ids below this millisecond are all used */
	int64		last_clock;		/* highest reading of the clock; not logged */
} FormData_regionseq_data;

typedef FormData_regionseq_data *Form_regionseq_data;

/* GUCs */
static int	regionseq_region_id = -1;
static int	regionseq_max_regression = 60000;

PG_FUNCTION_INFO_V1(regionseq_sequenceam_handler);

/* ---------------------------------------------------------------------
 * Helpers
 * ---------------------------------------------------------------------
 */

static int64
regionseq_now_ms(void)
{
	/* GetCurrentTimestamp() is in microseconds since 2000-01-01 */
	return GetCurrentTimestamp() / 1000 + INT64CONST(946684800000) -
		REGIONSEQ_EPOCH_MS;
}

static int
regionseq_timeline(void)
{
	/* a temporary sequence can be used on a standby; nothing is logged there */
	if (RecoveryInProgress())
		return 0;
	return (int) (GetWALInsertionTimeLine() & REGIONSEQ_TIMELINE_MASK);
}

static int64
regionseq_make_id(int64 ms, int region, int timeline, int32 counter)
{
	return (ms << REGIONSEQ_TIME_SHIFT) |
		((int64) region << REGIONSEQ_REGION_SHIFT) |
		((int64) timeline << REGIONSEQ_TIMELINE_SHIFT) | counter;
}

static Form_regionseq_data
read_regionseq(Relation rel, Buffer *buf, HeapTuple seqdatatuple)
{
	regionseq_magic *sm;
	Form_regionseq_data seq;

	SEQUENCE_PAGE_READ(Form_regionseq_data, regionseq_magic, REGIONSEQ_MAGIC);
	return seq;
}

/*
 * Write the state to the WAL as a full page image, with "image_*" as the
 * content of the tuple.  The buffer must be locked exclusively and the page
 * holds its tuple; on return the buffer holds the image, which the caller
 * then overwrites with the real state, as nextval of a plain sequence does.
 */
static void
regionseq_log_state(Relation rel, Buffer buf, int64 image_last_ms,
					int32 image_counter, bool image_is_called,
					int64 image_high_water)
{
	GenericXLogState *state;
	Page		image;
	ItemId		lp;
	HeapTupleHeader htup;
	Form_regionseq_data seq;

	/* so that the commit of whoever uses the ids waits for this record */
	GetTopTransactionId();

	state = GenericXLogStart(rel);
	image = GenericXLogRegisterBuffer(state, buf, GENERIC_XLOG_FULL_IMAGE);
	lp = PageGetItemId(image, FirstOffsetNumber);
	htup = (HeapTupleHeader) PageGetItem(image, lp);
	seq = (Form_regionseq_data) ((char *) htup + htup->t_hoff);
	seq->last_ms = image_last_ms;
	seq->counter = image_counter;
	seq->is_called = image_is_called;
	seq->high_water = image_high_water;
	seq->last_clock = 0;		/* what the WAL holds says nothing of the clock */
	GenericXLogFinish(state);
}

/*
 * Initialize the relation's page with the given tuple.  A temporary sequence
 * is not logged.
 */
static void
fill_regionseq(Relation rel, HeapTuple tuple)
{
	Buffer		buf;
	Page		page;
	regionseq_magic *sm;
	OffsetNumber offnum;
	ForkNumber	forkNum = MAIN_FORKNUM;
	GenericXLogState *state = NULL;

	SEQUENCE_PAGE_INIT(regionseq_magic, REGIONSEQ_MAGIC);

	if (RelationNeedsWAL(rel))
	{
		GetTopTransactionId();
		state = GenericXLogStart(rel);
	}

	START_CRIT_SECTION();

	MarkBufferDirty(buf);

	offnum = PageAddItem(page, tuple->t_data, tuple->t_len,
						 InvalidOffsetNumber, false, false);
	if (offnum != FirstOffsetNumber)
		elog(ERROR, "failed to add sequence tuple to page");

	if (state)
	{
		(void) GenericXLogRegisterBuffer(state, buf, GENERIC_XLOG_FULL_IMAGE);
		GenericXLogFinish(state);
	}

	END_CRIT_SECTION();

	UnlockReleaseBuffer(buf);
}

/* ---------------------------------------------------------------------
 * Callbacks
 * ---------------------------------------------------------------------
 */

static const char *
regionseq_get_table_am(void)
{
	return "heap";
}

static void
regionseq_check_persistence(char relpersistence)
{
	if (relpersistence == RELPERSISTENCE_UNLOGGED)
		ereport(ERROR,
				(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
				 errmsg("sequences of the regionseq access method cannot be unlogged"),
				 errdetail("The state must survive a crash for the ids to stay increasing.")));
}

static void
regionseq_init(Relation rel, int64 last_value, bool is_called)
{
	Datum		values[5];
	bool		nulls[5] = {false, false, false, false, false};
	List	   *atcmds = NIL;
	TupleDesc	tupdesc;
	HeapTuple	tuple;
	int64		ms = (last_value >> REGIONSEQ_TIME_SHIFT) & REGIONSEQ_TIME_MASK;

	regionseq_check_persistence(rel->rd_rel->relpersistence);

	/*
	 * START WITH of a plain sequence (1 by default) means nothing for ids
	 * that come from the clock: it is read as an id of the first millisecond
	 * of 2026, which any clock is past.
	 */
	{
		const char *names[5] = {"last_ms", "counter", "is_called", "high_water", "last_clock"};
		Oid			types[5] = {INT8OID, INT4OID, BOOLOID, INT8OID, INT8OID};

		for (int i = 0; i < 5; i++)
		{
			ColumnDef  *coldef = makeColumnDef(names[i], types[i], -1, InvalidOid);
			AlterTableCmd *atcmd = makeNode(AlterTableCmd);

			coldef->is_not_null = true;
			atcmd->subtype = AT_AddColumnToSequence;
			atcmd->def = (Node *) coldef;
			atcmds = lappend(atcmds, atcmd);
		}
	}
	AlterTableInternal(RelationGetRelid(rel), atcmds, false);
	CommandCounterIncrement();

	values[0] = Int64GetDatum(ms);
	values[1] = Int32GetDatum((int32) (last_value & REGIONSEQ_COUNTER_MASK));
	values[2] = BoolGetDatum(is_called);
	values[3] = Int64GetDatum(ms);
	values[4] = Int64GetDatum(ms);

	tupdesc = RelationGetDescr(rel);
	tuple = heap_form_tuple(tupdesc, values, nulls);
	fill_regionseq(rel, tuple);
}

static int64
regionseq_nextval(Relation rel, int64 incby, int64 maxv, int64 minv,
				  int64 cache, bool cycle, int64 *last)
{
	Buffer		buf;
	HeapTupleData seqdatatuple;
	Form_regionseq_data seq;
	Page		page;
	int64		now;
	int64		ms;
	int32		counter;
	int64		id;
	int64		high_water;
	bool		logit;

	if (regionseq_region_id < 0)
		ereport(ERROR,
				(errcode(ERRCODE_OBJECT_NOT_IN_PREREQUISITE_STATE),
				 errmsg("regionseq.region_id is not set"),
				 errhint("Set it in postgresql.conf to a number from 0 to 63, different in each region.")));

	if (incby != 1 || cache != 1)
		ereport(ERROR,
				(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
				 errmsg("sequence \"%s\" of the regionseq access method does not support INCREMENT or CACHE",
						RelationGetRelationName(rel))));

	now = regionseq_now_ms();
	seq = read_regionseq(rel, &buf, &seqdatatuple);
	page = BufferGetPage(buf);

	/*
	 * The clock, not the ids: a region that makes ids faster than 4,096 a
	 * millisecond runs ahead of the clock and that is fine; a clock that is
	 * behind its own last reading is not.
	 */
	if (seq->last_clock - now > regionseq_max_regression)
		ereport(ERROR,
				(errcode(ERRCODE_OBJECT_NOT_IN_PREREQUISITE_STATE),
				 errmsg("the clock is " INT64_FORMAT " ms behind its last reading for sequence \"%s\"",
						seq->last_clock - now, RelationGetRelationName(rel)),
				 errhint("regionseq.max_clock_regression is %d ms.",
						 regionseq_max_regression)));

	if (seq->is_called)
	{
		if (now > seq->last_ms)
		{
			ms = now;
			counter = 0;
		}
		else
		{
			ms = seq->last_ms;
			counter = seq->counter + 1;
			if (counter > REGIONSEQ_COUNTER_MASK)
			{
				/* take the next millisecond rather than wait for it */
				ms++;
				counter = 0;
			}
		}
	}
	else
	{
		/* setval(..., false): the stored value is the first one to hand out */
		if (now > seq->last_ms)
		{
			ms = now;
			counter = 0;
		}
		else
		{
			ms = seq->last_ms;
			counter = seq->counter;
		}
	}

	if (ms > REGIONSEQ_TIME_MASK)
		ereport(ERROR,
				(errcode(ERRCODE_SEQUENCE_GENERATOR_LIMIT_EXCEEDED),
				 errmsg("sequence \"%s\" has used all its 41 bits of milliseconds",
						RelationGetRelationName(rel))));

	id = regionseq_make_id(ms, regionseq_region_id, regionseq_timeline(), counter);

	/* the sequence's own MAXVALUE, which is 2147483647 under an int column */
	if (maxv >= 0 && id > maxv)
		ereport(ERROR,
				(errcode(ERRCODE_SEQUENCE_GENERATOR_LIMIT_EXCEEDED),
				 errmsg("nextval: reached maximum value of sequence \"%s\" (" INT64_FORMAT ")",
						RelationGetRelationName(rel), maxv),
				 errhint("Ids of the regionseq access method are 63 bits wide; the column must be bigint.")));

	/*
	 * Log a new high-water mark when the ids reach the last one, and for the
	 * first id after a checkpoint, as a plain sequence does: replay starting
	 * from the checkpoint has to find a record that covers the ids made
	 * since.
	 */
	high_water = seq->high_water;
	logit = false;
	if (RelationNeedsWAL(rel))
	{
		if (ms >= high_water)
			logit = true;
		else if (PageGetLSN(page) <= GetRedoRecPtr())
			logit = true;
	}
	else if (ms >= high_water)
		high_water = ms + REGIONSEQ_LEAD_MS;

	if (logit)
	{
		if (ms >= high_water)
			high_water = ms + REGIONSEQ_LEAD_MS;

		/* what the WAL says: everything below the high-water mark is used */
		regionseq_log_state(rel, buf, high_water - 1, REGIONSEQ_COUNTER_MASK,
							true, high_water);
	}

	START_CRIT_SECTION();
	MarkBufferDirty(buf);
	seq->last_ms = ms;
	seq->counter = counter;
	seq->is_called = true;
	seq->high_water = high_water;
	if (now > seq->last_clock)
		seq->last_clock = now;
	END_CRIT_SECTION();

	UnlockReleaseBuffer(buf);

	*last = id;
	return id;
}

static void
regionseq_setval(Relation rel, int64 next, bool iscalled)
{
	Buffer		buf;
	HeapTupleData seqdatatuple;
	Form_regionseq_data seq;
	int64		ms = (next >> REGIONSEQ_TIME_SHIFT) & REGIONSEQ_TIME_MASK;
	int32		counter = (int32) (next & REGIONSEQ_COUNTER_MASK);

	seq = read_regionseq(rel, &buf, &seqdatatuple);

	/* the exact state asked for is what the WAL gets */
	if (RelationNeedsWAL(rel))
		regionseq_log_state(rel, buf, ms, counter, iscalled, ms);

	START_CRIT_SECTION();
	MarkBufferDirty(buf);
	seq->last_ms = ms;
	seq->counter = counter;
	seq->is_called = iscalled;
	seq->high_water = ms;
	seq->last_clock = ms;
	END_CRIT_SECTION();

	UnlockReleaseBuffer(buf);
}

static void
regionseq_get_state(Relation rel, int64 *last_value, bool *is_called,
					XLogRecPtr *page_lsn)
{
	Buffer		buf;
	HeapTupleData seqdatatuple;
	Form_regionseq_data seq;

	seq = read_regionseq(rel, &buf, &seqdatatuple);

	/*
	 * An id once one has been made, so that pg_sequences shows what the
	 * column got.  Before that, the position without a region, which stays
	 * under the MAXVALUE of any column type: a 32-bit column fails in
	 * nextval, with its own message, not when it is created.
	 */
	*last_value = regionseq_make_id(seq->last_ms,
									(seq->is_called && regionseq_region_id > 0) ?
									regionseq_region_id : 0,
									seq->is_called ? regionseq_timeline() : 0,
									seq->counter);
	*is_called = seq->is_called;
	*page_lsn = PageGetLSN(BufferGetPage(buf));

	UnlockReleaseBuffer(buf);
}

static void
regionseq_reset(Relation rel, int64 startv, bool is_called, bool reset_state)
{
	Buffer		buf;
	HeapTupleData seqdatatuple;
	HeapTuple	tuple;
	Form_regionseq_data seq;
	int64		ms = (startv >> REGIONSEQ_TIME_SHIFT) & REGIONSEQ_TIME_MASK;

	(void) read_regionseq(rel, &buf, &seqdatatuple);
	tuple = heap_copytuple(&seqdatatuple);
	UnlockReleaseBuffer(buf);

	seq = (Form_regionseq_data) GETSTRUCT(tuple);
	seq->last_ms = ms;
	seq->counter = (int32) (startv & REGIONSEQ_COUNTER_MASK);
	seq->is_called = is_called;
	seq->high_water = ms;
	seq->last_clock = ms;

	RelationSetNewRelfilenumber(rel, rel->rd_rel->relpersistence);
	fill_regionseq(rel, tuple);
}

static void
regionseq_change_persistence(Relation rel, char newrelpersistence)
{
	Buffer		buf;
	HeapTupleData seqdatatuple;
	HeapTuple	tuple;

	regionseq_check_persistence(newrelpersistence);

	(void) read_regionseq(rel, &buf, &seqdatatuple);
	tuple = heap_copytuple(&seqdatatuple);
	UnlockReleaseBuffer(buf);

	RelationSetNewRelfilenumber(rel, newrelpersistence);
	fill_regionseq(rel, tuple);
}

static const SequenceAmRoutine regionseq_methods = {
	.type = T_SequenceAmRoutine,
	.get_table_am = regionseq_get_table_am,
	.init = regionseq_init,
	.nextval = regionseq_nextval,
	.setval = regionseq_setval,
	.get_state = regionseq_get_state,
	.reset = regionseq_reset,
	.change_persistence = regionseq_change_persistence
};

Datum
regionseq_sequenceam_handler(PG_FUNCTION_ARGS)
{
	PG_RETURN_POINTER(&regionseq_methods);
}

void
_PG_init(void)
{
	if (!process_shared_preload_libraries_in_progress)
		ereport(ERROR,
				(errcode(ERRCODE_OBJECT_NOT_IN_PREREQUISITE_STATE),
				 errmsg("regionseq must be loaded with shared_preload_libraries"),
				 errdetail("The region number is a setting of the server and is fixed for its lifetime.")));

	DefineCustomIntVariable("regionseq.region_id",
							"Region number put in the ids of regionseq sequences.",
							"0 to 63, different in each region; -1 means not set, and nextval fails.",
							&regionseq_region_id,
							-1, -1, REGIONSEQ_REGION_MASK,
							PGC_POSTMASTER,
							0, NULL, NULL, NULL);

	DefineCustomIntVariable("regionseq.max_clock_regression",
							"How far the clock may be behind its last reading before nextval fails.",
							"Ids stay increasing while the clock is behind, at the pace of 4,096 a millisecond.",
							&regionseq_max_regression,
							60000, 0, INT_MAX,
							PGC_SUSET,
							GUC_UNIT_MS, NULL, NULL, NULL);

	MarkGUCPrefixReserved("regionseq");
}
