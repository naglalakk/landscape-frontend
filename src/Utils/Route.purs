module Utils.Route where 

import Data.Lens.Iso.Newtype            (_Newtype)
import Routing.Duplex                   (RouteDuplex'
                                        ,string, int
                                        ,segment)
import Slug                             as Slug

import Data.BlogPost                    (BlogPostId)
import Data.Image                       (ImageId)


blogPostId :: RouteDuplex' BlogPostId
blogPostId = _Newtype (int segment)

imageId :: RouteDuplex' ImageId
imageId = _Newtype (int segment)

slug :: RouteDuplex' String
slug = string segment

tag :: RouteDuplex' String
tag = string segment
