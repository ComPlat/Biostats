#include <R.h>
#include <Rinternals.h>
#include "_cgo_export.h"

typedef struct {
  GoString gs;
  char* data;
} GS;

// make gostring
GS mgs(SEXP size, SEXP source) {
  GS ret;
  int* size_p = INTEGER(size);
  ret.gs.n = size_p[0];
  ret.data = (char*)malloc(sizeof(char) * ret.gs.n);

  if(ret.data == NULL) {
    Rf_error("malloc error"); 
  }

  const char* p_sexp = CHAR(STRING_ELT(source, 0));
  for(int i = 0; i < ret.gs.n; i++) {
    ret.data[i] = p_sexp[i];
  }

  ret.gs.p = ret.data;

  return ret;
}

SEXP godownload(SEXP token_inp, SEXP url_inp, SEXP filename_inp, SEXP path_inp,
                SEXP token_size, SEXP url_size, SEXP filename_size, SEXP path_size) {
  
  
  GS token = mgs(token_size, token_inp);
  GS url = mgs(url_size, url_inp);
  GS filename = mgs(filename_size, filename_inp);
  GS path = mgs(path_size, path_inp);
  
  int ret = DownloadIt(token.gs, url.gs, filename.gs, path.gs);
  
  free(token.data);
  free(url.data);
  free(filename.data);
  free(path.data);

  return Rf_ScalarInteger( ret );
}


SEXP goupload(SEXP token_inp, SEXP url_inp, SEXP filepath_inp, SEXP id_inp, SEXP type_inp,
                     SEXP token_size, SEXP url_size, SEXP filepath_size, SEXP id_size, SEXP type_size) {

  
  GS token = mgs(token_size, token_inp);
  GS url = mgs(url_size, url_inp);
  GS filepath = mgs(filepath_size, filepath_inp);
  GS id = mgs(id_size, id_inp);
  GS type = mgs(type_size, type_inp);

  int ret = UploadIt(token.gs, url.gs, filepath.gs, id.gs, type.gs);

  free(token.data);
  free(url.data);
  free(filepath.data);
  free(id.data);
  free(type.data);

  return Rf_ScalarInteger( ret );
}

