{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module ModelExt(buildExt) where

import           Data.Aeson
import           Data.Text
import           Database.Persist.Postgresql
import           Model

class ExtBuilder c where
    type Target c
    buildExt :: Entity c -> SqlPersistT IO (Target c)

newtype ArticleTagExt = ArticleTagExt (Entity ArticleTag, User, Tag)

instance ExtBuilder ArticleTag where
    type Target ArticleTag = ArticleTagExt
    buildExt atag = do
        let atagVal = entityVal atag
        tag     <- belongsToJust articleTagTag atagVal
        adder   <- belongsToJust articleTagAdder atagVal
        return $ ArticleTagExt (atag, adder, tag)

instance ToJSON ArticleTagExt where
    toJSON (ArticleTagExt (atag, adder, tag)) = object
        [ "id"      .= entityKey atag
        , "article" .= articleTagArticle atagVal
        , "tag"     .= object
            [ "id"      .= articleTagTag atagVal
            , "name"    .= tagName tag ]
        , "adder"   .= object
            [ "id"      .= articleTagAdder atagVal
            , "name"    .= userUsername adder ]
        , "ctime"   .= articleTagCtime atagVal ] where
            atagVal = entityVal atag

newtype ArticleExt = ArticleExt (Entity Article, User, Node, [ArticleTagExt])

instance ExtBuilder Article where
    type Target Article = ArticleExt
    buildExt article = do
        let articleVal = entityVal article
        author  <- belongsToJust articleAuthor articleVal
        node    <- belongsToJust articleNode articleVal
        tags    <- mapM buildExt =<< selectList [ArticleTagArticle ==. (entityKey article)] []
        return $ ArticleExt (article, author, node, tags)

instance ToJSON ArticleExt where
    toJSON (ArticleExt (article, author, node, tags)) = object
        [ "id"      .= entityKey article
        , "title"   .= articleTitle articleVal
        , "author"  .= object
            [ "id"      .= articleAuthor articleVal
            , "name"    .= userUsername author ]
        , "node"    .= object
            [ "id"      .= articleNode articleVal
            , "name"    .= nodeName node ]
        , "type"    .= articleType articleVal
        , "content" .= articleContent articleVal
        , "tags"    .= tags
        , "ctime"   .= articleCtime articleVal
        , "etime"   .= articleEtime articleVal ] where
            articleVal = entityVal article

newtype CommentExt = CommentExt (Entity Comment, User)

instance ExtBuilder Comment where
    type Target Comment = CommentExt
    buildExt comment = do
        let commentVal = entityVal comment
        author  <- belongsToJust commentAuthor commentVal
        return $ CommentExt (comment, author)

instance ToJSON CommentExt where
    toJSON (CommentExt (comment, author)) = object
        [ "id"      .= entityKey comment
        , "target"  .= commentTarget commentVal
        , "author"  .= object
            [ "id"      .= commentAuthor commentVal
            , "name"    .= userUsername author ]
        , "content" .= commentContent commentVal
        , "ctime"   .= commentCtime commentVal ] where
            commentVal = entityVal comment
