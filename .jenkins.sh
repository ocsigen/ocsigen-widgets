opam pin add --no-action eliom 'https://github.com/ocsigen/eliom.git#master'
opam pin add --no-action ojquery 'https://github.com/ocsigen/ojquery.git#master'

opam pin add --no-action ocsigen-widgets .
opam install --deps-only ocsigen-widgets
opam install --verbose ocsigen-widgets

do_build_doc () {
  make doc
#  cp -Rf doc/manual/files/* ${MANUAL_FILES_DIR}/
#  cp -Rf doc/manual/src/* ${MANUAL_SRC_DIR}/
#  cp -Rf doc/api/wiki/*.wiki ${API_DIR}/
}

do_remove () {
  opam remove --verbose js_of_ocaml
}
