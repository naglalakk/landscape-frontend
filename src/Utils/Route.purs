module Utils.Route where 

import Data.Lens.Iso.Newtype            (_Newtype)
import Routing.Duplex                   (RouteDuplex'
                                        ,string, int
                                        ,segment)
import Slug                             as Slug

import Data.BlogPost                    (BlogPostId)
import Data.Exhibition (ExhibitionId)
import Data.Image                       (ImageId)
import Data.Item (ItemId)
import Data.Tag                         (TagId)
import Data.Token (TokenId)


blogPostId :: RouteDuplex' BlogPostId
blogPostId = _Newtype (int segment)

exhibitionId :: RouteDuplex' ExhibitionId
exhibitionId = _Newtype (int segment)

imageId :: RouteDuplex' ImageId
imageId = _Newtype (int segment)

itemId :: RouteDuplex' ItemId
itemId = _Newtype (int segment)

slug :: RouteDuplex' String
slug = string segment

tag :: RouteDuplex' String
tag = string segment

tagId :: RouteDuplex' TagId
tagId = _Newtype (int segment)

tokenId :: RouteDuplex' TokenId
tokenId = _Newtype (int segment)
