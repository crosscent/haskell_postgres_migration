# haskell_postgres_migration

Stupidly simple migration tool that reads the sql files in a directory and
executing them when appropriate.

This is a simplified version of the Django migrate tool. Not the makemigration
and migrate tool. JUST the migrate tool. It goes through the migration
directory and applies each of the migrations that has not been applied.

# Steps

* Put all your SQL migration files in migration/

* Either set your connection to PostgreSQL in the environment, or add the flags
  when executing the binary

* ./postgres-migration-exe --POSTGRES_USERNAME="username" --POSTGRES_PASSSWORD="password" --POSTGRES_SERVER="server" --POSTGRES_DBNAME="dbname"
