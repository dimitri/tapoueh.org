-- @service: warehouse
-- The new contact arrived without its email; the OLD emails stay where the initial copy put them.
select id, name, email from crm.contacts order by id;
