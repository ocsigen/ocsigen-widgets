opam pin add --no-action tyxml 'https://github.com/ocsigen/tyxml.git#master'
opam pin add --no-action eliom 'https://github.com/ocsigen/eliom.git#master'
#opam pin add --no-action deriving 'https://github.com/ocsigen/deriving.git#master'
opam pin add --no-action js_of_ocaml 'https://github.com/ocsigen/js_of_ocaml.git#master'
opam pin add --no-action ojquery 'https://github.com/ocsigen/ojquery.git#master'
opam pin add --no-action reactiveData 'https://github.com/ocsigen/reactiveData.git#master'
opam pin add --no-action text 'https://github.com/vasilisp/ocaml-text.git#optional-camlp4'

opam pin add --no-action ocsigen-widgets .
opam install --deps-only ocsigen-widgets
opam install --verbose ocsigen-widgets

do_build_doc () {
  make doc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  mkdir -p ${API_DIR}/server ${API_DIR}/client
  cp -Rf doc/server/wiki/*.wiki ${API_DIR}/server/
  cp -Rf doc/client/wiki/*.wiki ${API_DIR}/client/
#  cp -Rf doc/index.wiki ${API_DIR}/

}

do_remove () {
  opam remove --verbose ocsigen-widgets
}
