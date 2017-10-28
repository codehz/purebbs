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
- [ ] **GET**       list/node/:node
- [ ] **GET**       list/user/:user
- [ ] **GET**       list/tag/:tag
- [ ] **GET**       article/:article
- [ ] **DELETE**    article/:article
- [ ] **PUT**       article/:article
- [x] **POST**      article
- [ ] **POST**      comment
- [ ] **DELETE**    comment
- [ ] **POST**      tag
- [ ] **PUT**       tag
- [ ] **DELETE**    tag
- [ ] **POST**      tag/:article
- [ ] **DELETE**    tag/:article
