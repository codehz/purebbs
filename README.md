# purebbs
It is under development.

## API design
```
/api/v1:
POST    register
POST    login

/api/v1/realm: (need login)
GET     whoami
GET     node
POST    node
GET     node/:node
POST    node/:node
DELETE  node/:node
PUT     node/:node
GET     messages
PUT     messages/:id
GET     list/node/:node
GET     list/user/:user
GET     list/tag/:tag
GET     article/:article
DELETE  article/:article
PUT     article/:article
POST    article
POST    comment
DELETE  comment
POST    tag
PUT     tag
DELETE  tag
POST    tag/:article
DELETE  tag/:article

```
