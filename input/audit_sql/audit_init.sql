-- Create Audit Table
CREATE TABLE IF NOT EXISTS `cvt.cvt_audit` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fk_table_id` in(11) NOT NULL,
  `tbl_name` varchar(45) NOT NULL,
  `record` json NOT NULL,
  `qc_status` varchar(45) DEFAULT NULL,
  `qc_notes` varchar(200) DEFAULT NULL,
  `qc_flags` varchar(45) DEFAULT NULL,
  `version` int(11) NOT NULL DEFAULT '0',
  `rec_create_dt` datetime NOT NULL,
  `created_by` varchar(45) NOT NULL,
  `end_time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id_UNIQUE` (`id`),
  UNIQUE KEY `fk_table_id_version` (`fk_table_id`,`version`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='CvT audit table';

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
      src_tbl_name,
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
      'fail',
  	  OLD.qc_notes,
  	  OLD.qc_flags,
  	  OLD.version,
      OLD.rec_create_dt,
      OLD.created_by,
      current_timestamp()
      );

-- Drop trigger if need be
DROP TRIGGER IF EXISTS cvt_table_audit_bu;

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

-- Drop trigger if need be
DROP TRIGGER IF EXISTS cvt_table_update_bu;
