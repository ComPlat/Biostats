package interaction_with_ELN

import (
  _"crypto/tls"
  "io"
  "io/ioutil"
  _"net" 
  "net/http"
  "os"
  _"errors"
  "fmt"
  "bytes"
  "mime/multipart"
  "strings"
)

var p = fmt.Println;

// download file
func Download(token string, urlinp string, filename string, path string) error {

  //req, err := http.NewRequest("GET", urlinp, nil)
  req, err := http.NewRequest("GET", urlinp, nil)
  
  if err != nil {
    return err
  }
  
  token_ := "Bearer " + token
  req.Header.Set("Authorization", token_)
  
  resp, err := http.DefaultClient.Do(req)
  if err != nil {
    return (err)
  }
  defer resp.Body.Close()
  out, err := os.Create(path + filename)
  if err != nil {
    return (err)
  }
  defer out.Close()
  
  _, err = io.Copy(out, resp.Body)
  
  _, err = ioutil.ReadFile(path + filename)
  
  // otherwise shiny cannot read file...
  // 997 is user shiny or 999
  //err = os.Chown(path + filename, 999, 999)

  return (err)
  //return nil;
}


// upload to eln
func Upload(token string, urlinp string, filename string, id string, type_ string) error {

  // include the file which contains the output of Biostats to Form
  var body = &bytes.Buffer{}
  writer := multipart.NewWriter(body)

  fw, err := writer.CreateFormFile("file", filename)
  if err != nil {
    return (err)
  }

  file, err := ioutil.ReadFile(filename)
  if err != nil {
    return (err)
  }

  _, err = io.Copy(fw, bytes.NewReader(file))
  if err != nil {
    return (err)
  }

  // add attachable_id to Form
  fw, err = writer.CreateFormField("attachable_id")
  if err != nil {
    return err
  }
  _, err = io.Copy(fw, strings.NewReader(id))

  // add attachable_type to Form
  fw, err = writer.CreateFormField("attachable_type")
  if err != nil {
    return err
  }
  _, err = io.Copy(fw, strings.NewReader(type_))

  writer.Close()

  // define request --> https instead of http!!!!! Still to do
  //req, err := http.NewRequest("POST", "http://193.196.39.76/api/v1/attachments_jwt/upload_attachments", bytes.NewReader(body.Bytes()))
  req, err := http.NewRequest("POST", "http://127.0.0.1:3000/api/v1/attachments_jwt/upload_attachments", bytes.NewReader(body.Bytes()))
  if err != nil {
    return (err)
  }

  // define header
  token_ := "Bearer " + token
  req.Header.Set("Content-Type", "multipart/form-data")
  req.Header.Set("Accept", "application/json")
  req.Header.Set("Authorization", token_)
  req.Header.Set("Content-Type", writer.FormDataContentType())

  // send stuff to ELN
  resp, err := http.DefaultClient.Do(req)
  if err != nil {
    return err
  }
  defer resp.Body.Close()

  // delete file
  err = os.Remove(filename)
  if err != nil {
    return err
  }
  
  return err
}
