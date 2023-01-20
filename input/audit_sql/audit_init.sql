create table cvt.cvt_audit
(
    id            integer                    not null
        constraint cvt_audit_pk
            primary key,
    fk_table_id   integer                    not null,
    fk_table_name text                       not null,
    record        json                       not null,
    qc_status     varchar(45) default 'FAIL' not null,
    qc_flags      text        default NULL,
    qc_notes      text        default NULL,
    version       integer                    not null,
    rec_create_dt timestamp                  not null,
    rec_end_dt    timestamp   default now()  not null,
    created_by    varchar(45)                not null,
    constraint cvt_unq_tbl_id_name_version
        unique (fk_table_id, fk_table_name, version)
);

comment on table cvt.cvt_audit is 'CvT audit table';

comment on column cvt.cvt_audit.fk_table_id is 'Foreign key for id of the table the audit record is associated with';

comment on column cvt.cvt_audit.fk_table_name is 'Name of the table the fk_table_id is from';

comment on column cvt.cvt_audit.record is 'JSON record for the field-values audited';

comment on column cvt.cvt_audit.qc_status is 'Quality control status tag';

comment on column cvt.cvt_audit.qc_flags is 'Quality control flags';

comment on column cvt.cvt_audit.qc_notes is 'Quality control review notes';

comment on column cvt.cvt_audit.version is 'Version count for the record';

comment on column cvt.cvt_audit.rec_create_dt is 'Datetime stamp when record was created';

comment on column cvt.cvt_audit.rec_end_dt is 'Datetime stamp when record was audited';

comment on column cvt.cvt_audit.created_by is 'User who created the record';

comment on constraint cvt_unq_tbl_id_name_version on cvt.cvt_audit is 'Enforce unique key for foreign table ID, name, and version';

-- Trigger for Before Update on dataset_details table
-- https://mysql-0v34c10ck.blogspot.com/2011/06/how-to-disableenable-triggers-on-demand.html
CREATE TRIGGER cvt_table_audit_bu BEFORE UPDATE ON cvt.cvt_table
  FOR EACH ROW thisTrigger: BEGIN
    IF ((@TRIGGER_CHECKS = FALSE)) THEN
      LEAVE thisTrigger;
    END IF;

    INSERT INTO cvt_audit
      (
  	  fk_table_id,
      fk_table_name,
      record,
      qc_status,
  	  qc_notes,
  	  qc_flags,
  	  version,
      rec_create_dt,
      created_by,
      end_time
      )
      VALUES
      (
      OLD.id,
      'cvt_table',
      JSON_OBJECT(),
      'FAIL',
  	  OLD.qc_notes,
  	  OLD.qc_flags,
  	  OLD.version,
      OLD.rec_create_dt,
      OLD.created_by,
      now()
      );

-- Drop trigger if need be
DROP TRIGGER IF EXISTS cvt_table_audit_bu ON cvt_table;

-- BEFORE UPDATE trigger
CREATE TRIGGER cvt_table_update_bu BEFORE UPDATE ON cvt.cvt_table_update
  FOR EACH ROW thisTrigger: BEGIN
    IF ((@TRIGGER_CHECKS = FALSE)) THEN
      LEAVE thisTrigger;
    END IF;

    SET NEW.version = OLD.version + 1;
      IF NEW.created_by is null THEN
        SET NEW.created_by = USER();
      END IF;

-- Updated postgreSQL version, still need to check user defined variable boolean
CREATE TRIGGER cvt_table_update_bu
AFTER INSERT OR UPDATE ON cvt.documents
FOR EACH ROW
WHEN (current_setting('TRIGGER_CHECKS'))
EXECUTE PROCEDURE increment_version_set_user();

CREATE FUNCTION increment_version_set_user() RETURNS trigger AS
    $$
BEGIN
    NEW.version := OLD.version + 1;
    IF NEW.created_by is null THEN
        SET NEW.created_by = USER;
    END IF;
    RETURN NEW;
END; $$ LANGUAGE plpgsql;

-- Drop trigger if need be
DROP TRIGGER IF EXISTS cvt_table_update_bu ON cvt_table;
