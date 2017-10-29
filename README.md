<p align="center">
<img src=https://gitcdn.xyz/repo/codehz/purebbs/master/logo.svg>
<h3 align="center">Pure BBS</h3>
</p>
<p align="center">
A simple bulletin board system writen by Haskell.<br>
<b>The project is not yet complete</b>
</p>

## API design

### /api/v1:
- [x] **POST** register
- [x] **POST** login

### /api/v1/realm: (need login)
- [x] **GET**       whoami
- [x] **PUT**       user
- [x] **GET**       node
- [x] **POST**      node
- [x] **GET**       node/:node
- [x] **POST**      node/:node
- [x] **DELETE**    node/:node
- [x] **PUT**       node/:node
- [x] **GET**       messages
- [x] **PUT**       messages/:id
- [x] **GET**       list
- [x] **GET**       list/node/:node
- [x] **GET**       list/user/:user
- [x] **GET**       list/tag/:tag
- [x] **GET**       article/:article
- [x] **DELETE**    article/:article
- [x] **PUT**       article/:article
- [x] **POST**      article
- [x] **GET**       article/:article/comment
- [x] **POST**      article/:article/comment
- [x] **DELETE**    comment/:comment
- [x] **POST**      tag
- [x] **PUT**       tag/:tag
- [ ] **DELETE**    tag/:tag
- [ ] **POST**      tag/:article
- [ ] **DELETE**    tag/:article
