# Plan

install.packages("./app/COMELN", type = "source", repos = NULL)
.rs.restartR()

# http://localhost:3000/api/v1/attachments_jwt/14
folder <- "/home/konrad/Documents/GitHub/shinychem/stuff"
url <- "http://localhost:3000/api/v1/attachments_jwt/14"
token <- "eyJhbGciOiJIUzI1NiJ9.eyJjbGllbnRfaWQiOm51bGwsImN1cnJlbnRfdXNlcl9pZCI6MTEsImV4cCI6MTY2OTM4MjgwNn0.G8_nZjSKTmz7HH_6oIrp2XuzNgLGu26ifqjC2HeI41s"
gotest::download(token, url, file, folder)


