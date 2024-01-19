# audit_testing
# Script to test if audit triggers are working as expected
# Jonathan Taylor Wall
# 2023-12-21


# Check list of tables in database
tbl_list = db_query_cvt("SELECT table_name FROM information_schema.tables WHERE table_schema = 'cvt'")
# Check list of triggers in database
trigger_list = db_query_cvt("select * from information_schema.triggers")
# Check list of functions in database
db_func_list = db_query_cvt("SELECT * FROM information_schema.routines where routine_schema = 'cvt'")

# Check if audit table is empty
audit = db_query_cvt("SELECT * FROM cvt.cvt_audit")

# Pull current table record
curr = db_query_cvt("SELECT * FROM cvt.documents where id = 25049")

# Make update to Documents table
db_query_cvt("UPDATE cvt.documents SET qc_notes = 'audit_test' WHERE id in (25049)")

# Pull new table record
new = db_query_cvt("SELECT * FROM cvt.documents where id = 25049")

