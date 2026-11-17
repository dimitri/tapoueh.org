# Copyright (c) 2026, PostgreSQL Global Development Group

regionseq_sources = files(
  'regionseq.c',
)

if host_system == 'windows'
  regionseq_sources += rc_lib_gen.process(win32ver_rc, extra_args: [
    '--NAME', 'regionseq',
    '--FILEDESC', 'regionseq - sequence access method',])
endif

regionseq = shared_module('regionseq',
  regionseq_sources,
  kwargs: contrib_mod_args,
)
contrib_targets += regionseq

install_data(
  'regionseq.control',
  'regionseq--1.0.sql',
  kwargs: contrib_data_args,
)

tests += {
  'name': 'regionseq',
  'sd': meson.current_source_dir(),
  'bd': meson.current_build_dir(),
  'regress': {
    'sql': [
      'regionseq',
    ],
    'regress_args': ['--temp-config', files('regionseq.conf')],
  },
}
