#!/bin/sh
set -eu

ENV_FILE=/etc/etl/container.env

mkdir -p "$(dirname "$ENV_FILE")" /var/log
: > "$ENV_FILE"

printenv | while IFS='=' read -r name value; do
  case "$name" in
    ''|*[!A-Za-z0-9_]*|[0-9]*)
      continue
      ;;
  esac

  escaped_value=$(printf "%s" "$value" | sed "s/'/'\\\\''/g")
  printf "export %s='%s'\n" "$name" "$escaped_value" >> "$ENV_FILE"
done

touch /var/log/etl.log

exec "$@"
