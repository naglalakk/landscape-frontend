module Utils.Route where 

import Data.Lens.Iso.Newtype            (_Newtype)
import Routing.Duplex                   (RouteDuplex'
                                        ,string, int
                                        ,segment)
import Slug                             as Slug

import Data.BlogPost                    (BlogPostId)


blogPostId :: RouteDuplex' BlogPostId
blogPostId = _Newtype (int segment)

slug :: RouteDuplex' String
slug = string segment
