esc=$(printf '\033')
refmt -parse re -print ml node_modules/jengaboot/jengaroot.re \
  | sed 's/\[@explicit_arity \]//g' \
  > node_modules/jengaboot/jengaroot.ml \
  && (time -p jenga -path-to-jenga-conf ./node_modules/jengaboot/jenga.conf \
  -show-actions-run-verbose -verbose -show-checked \
  -show-error-dependency-paths -brief-error-summary -show-buildable-discovery \
  -show-reflecting -show-considering -trace) \
  | sed "s/exit_code = 0/${esc}[32m&${esc}[39m/;
  s/exit_code = 1/${esc}[31m&${esc}[0m/;
  s/\*\*\* /${esc}[30m> ${esc}[0m/;
  s/jenga: ERROR:/${esc}[31m&${esc}[0m/;
  s/- build/${esc}[30m> ${esc}[0m${esc}[40m${esc}[33mbuild${esc}[0m/;
  s/+ bash/${esc}[30m> ${esc}[0m${esc}[40m${esc}[33mbash${esc}[0m/;
  s/- exit.*/${esc}[30m> ${esc}[30m&${esc}[0m/;
  s/jenga: NOT RUNNING:/${esc}[40m${esc}[34m&${esc}[0m/;
  s/jenga: Considering:.*/${esc}[30m&${esc}[0m/;
  s/jenga: Building:/${esc}[0m${esc}[40m${esc}[33m&${esc}[0m/;
  s/jenga: //;
  s/bash -c.*/${esc}[3m&${esc}[0m/"
