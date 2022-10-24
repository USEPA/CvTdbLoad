---- Delete fields
ALTER TABLE cvt.tk_parameters DROP COLUMN loglikelihood;
ALTER TABLE cvt.tk_parameters DROP COLUMN aic;
ALTER TABLE cvt.tk_parameters DROP COLUMN joint_analysis_studies;

---- Add new fields
-- Add tk_model field (name of model)
alter table cvt.tk_parameters
    add tk_model varchar(25) default NULL;
comment on column cvt.tk_parameters.tk_model is 'Name of tk model';

-- TK param ID version (ID-version pair)
alter table cvt.tk_parameters
    add version integer not null;
comment on column cvt.tk_parameters.version is 'tk param ID version (ID-version pair)';

-- Add rmse (root mean square error) - Values usually 0.0-1.0
alter table cvt.tk_parameters
    add rmse numeric default NULL;
comment on column cvt.tk_parameters.rmse is 'root mean square error - Values usually 0.0-1.0';

-- Add invivopkfit model version (software version format - e.g., 1.0.0)
alter table cvt.tk_parameters
    add invivopkfit_version varchar(10) default NULL;
comment on column cvt.tk_parameters.invivopkfit_version is 'software version format - e.g., 1.0.0';

-- Add boolean flag omit from ccd (0 or 1)
alter table cvt.tk_parameters
    add omit_from_ccd smallint default 0;
comment on column cvt.tk_parameters.omit_from_ccd is 'boolean flag omit from ccd (0 or 1)';

---- Create new tk_params_series linker table
create table cvt.tk_parameters_series
(
	id serial,
	fk_tk_parameters_id int not null
		constraint tk_parameters_series_tk_parameters_id_fk
			references cvt.tk_parameters,
	version int not null,
	fk_series_id int not null
		constraint tk_parameters_series_series_id_fk
			references cvt.series,
	created_by varchar(50) not null,
	updated_by varchar(50),
	rec_create_dt timestamp default current_timestamp not null,
	rec_update_dt timestamp
);

comment on table cvt.tk_parameters_series is 'Linker table between tk_parameters and series table';

comment on column cvt.tk_parameters_series.fk_tk_parameters_id is 'Foreign key to tk_parameters ID';

comment on column cvt.tk_parameters_series.version is 'Version of the tk_params set (fk_tk_parameters)';

comment on column cvt.tk_parameters_series.fk_series_id is 'Foreign key to the series ID';

comment on column cvt.tk_parameters_series.created_by is 'User who created the record';

comment on column cvt.tk_parameters_series.updated_by is 'User who updated the record';

comment on column cvt.tk_parameters_series.rec_create_dt is 'Timestamp when record was created';

comment on column cvt.tk_parameters_series.rec_update_dt is 'Timestamp when record was updated';

create unique index tk_parameters_series_id_uindex
	on cvt.tk_parameters_series (id);

alter table cvt.tk_parameters_series
	add constraint tk_parameters_series_pk
		primary key (id);

---- Create new tk_params_studies linker table
create table cvt.tk_parameters_studies
(
	id serial,
	fk_tk_parameters_id int not null
		constraint tk_parameters_studies_tk_parameters_id_fk
			references cvt.tk_parameters,
	version int not null,
	fk_studies_id int not null
		constraint tk_parameters_studies_studies_id_fk
			references cvt.studies,
	created_by varchar(50) not null,
	updated_by varchar(50),
	rec_create_dt timestamp default current_timestamp not null,
	rec_update_dt timestamp
);

comment on table cvt.tk_parameters_studies is 'Linker table between tk_parameters and studies table';

comment on column cvt.tk_parameters_studies.fk_tk_parameters_id is 'Foreign key to tk_parameters ID';

comment on column cvt.tk_parameters_studies.version is 'Version of the tk_params set (fk_tk_parameters)';

comment on column cvt.tk_parameters_studies.fk_studies_id is 'Foreign key to the studies ID';

comment on column cvt.tk_parameters_studies.created_by is 'User who created the record';

comment on column cvt.tk_parameters_studies.updated_by is 'User who updated the record';

comment on column cvt.tk_parameters_studies.rec_create_dt is 'Timestamp when record was created';

comment on column cvt.tk_parameters_studies.rec_update_dt is 'Timestamp when record was updated';

create unique index tk_parameters_studies_id_uindex
	on cvt.tk_parameters_studies (id);

alter table cvt.tk_parameters_studies
	add constraint tk_parameters_studies_pk
		primary key (id);
