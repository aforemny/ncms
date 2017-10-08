module Ncms.Github exposing
    (
      User
    , defaultUser
    , getUser

    , Reference
    , Object
    , getReference
    , updateReference

    , Tree
    , defaultTree
    , File
    , defaultFile
    , getTree
    , getTreeRecursively
    , createTree

    , Blob
    , defaultBlob
    , getBlob
    , createBlob

    , Commit
    , defaultCommit
    , Author
    , defaultAuthor
    , getCommit
    , createCommit
    )

import Ncms.Github.Blob
import Ncms.Github.Commit
import Ncms.Github.Reference
import Ncms.Github.Tree
import Ncms.Github.User


-- TREE


type alias Tree a =
    Ncms.Github.Tree.Tree a


defaultTree =
    Ncms.Github.Tree.defaultTree


type alias File =
    Ncms.Github.Tree.File


defaultFile =
    Ncms.Github.Tree.defaultFile


getTree =
    Ncms.Github.Tree.getTree


getTreeRecursively =
    Ncms.Github.Tree.getTreeRecursively


createTree =
    Ncms.Github.Tree.createTree


-- USER


type alias User =
    Ncms.Github.User.User


defaultUser =
    Ncms.Github.User.defaultUser


getUser =
    Ncms.Github.User.getUser


type alias Reference =
    Ncms.Github.Reference.Reference


defaultReference =
    Ncms.Github.Reference.defaultReference


type alias Object =
    Ncms.Github.Reference.Object


defaultObject =
    Ncms.Github.Reference.Object


getReference =
    Ncms.Github.Reference.getReference


updateReference =
    Ncms.Github.Reference.updateReference


type alias Blob =
    Ncms.Github.Blob.Blob


defaultBlob =
    Ncms.Github.Blob.defaultBlob


getBlob =
    Ncms.Github.Blob.getBlob


createBlob =
    Ncms.Github.Blob.createBlob


-- COMMIT


type alias Commit =
    Ncms.Github.Commit.Commit


defaultCommit =
    Ncms.Github.Commit.defaultCommit


type alias Author =
    Ncms.Github.Commit.Author


defaultAuthor =
    Ncms.Github.Commit.defaultAuthor


getCommit =
    Ncms.Github.Commit.getCommit


createCommit =
    Ncms.Github.Commit.createCommit
