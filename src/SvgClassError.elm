module SvgClassError exposing (main)

import Html exposing (Html)
import Html.Attributes
import Svg
import Svg.Attributes


main : Html Never
main =
    Svg.svg
        [ -- Remove this line and it won't throw exception
          Html.Attributes.class "bi"
        , Html.Attributes.width 32
        , Html.Attributes.height 32
        ]
        [ Svg.use [ Svg.Attributes.xlinkHref "bootstrap-icons.svg#heart-fill" ] []
        ]
