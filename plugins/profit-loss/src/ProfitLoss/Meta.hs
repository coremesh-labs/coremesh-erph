-- | Metamodell (Catalog.Describe): Modul GuV mit dem Bericht ProfitLoss – ein
-- Object ohne Übersicht; der WebServer zeigt die Maske der Aktion compute,
-- darunter das Ergebnis (Tabelle, CSV zum Speichern).
module ProfitLoss.Meta
  ( describe
  , moduleName
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as K
import Data.Text (Text)

moduleName :: Text
moduleName = "profit-loss"

key :: Text -> Text
key k = moduleName <> ".ProfitLoss." <> k

-- | (Feld, Label, Typ, Pflicht, Lookup)
fields :: [(Text, Text, Text, Bool, Maybe Value)]
fields =
  [ ("company_code", "Buchungskreis", "text", True, Just (object ["object" .= ("CompanyCode" :: Text), "value_field" .= ("code" :: Text), "label_fields" .= ["description" :: Text]]))
  , ( "rent_object_id"
    , "Mietobjekt (leer = alle)"
    , "text"
    , False
    , Just
        ( object
            [ "object" .= ("RentObject" :: Text)
            , "value_field" .= ("object_id" :: Text)
            , "label_fields" .= ["designation" :: Text]
            , "filters" .= object ["company_code" .= ("company_code" :: Text)]
            ]
        )
    )
  , ("period_from", "Zeitraum von", "date", True, Nothing)
  , ("period_to", "Zeitraum bis", "date", True, Nothing)
  ]

describe :: Value
describe =
  object
    [ "objects"
        .= [ object
               [ "name" .= ("ProfitLoss" :: Text)
               , "title" .= ("GuV-Rechnung" :: Text)
               , "title_key" .= key "title"
               , "icon" .= ("icon-chart" :: Text)
               , "form_state" .= ("formState" :: Text)
               , "lifecycle" .= object ["type" .= ("immutable" :: Text)]
               , "fields"
                   .= [ object
                          ( [ "key" .= k
                            , "label" .= l
                            , "label_key" .= key ("fields." <> k)
                            , "type" .= t
                            , "required" .= r
                            , "editable" .= True
                            , "listable" .= False
                            , "action_only" .= True
                            ]
                              <> maybe [] (\v -> ["lookup" .= v]) lk
                          )
                      | (k, l, t, r, lk) <- fields
                      ]
               , "actions"
                   .= [ object
                          [ "name" .= ("compute" :: Text)
                          , "kind" .= ("custom" :: Text)
                          , "label" .= ("GuV berechnen" :: Text)
                          , "label_key" .= key "actions.compute"
                          , "fields" .= [k | (k, _, _, _, _) <- fields]
                          , "form_state" .= True
                          ]
                      ]
               ]
           ]
    , "modules"
        .= [ object
               [ "name" .= moduleName
               , "title" .= ("GuV" :: Text)
               , "title_key" .= (moduleName <> ".module.title")
               , "icon" .= ("icon-chart" :: Text)
               , "description" .= ("Gewinn- und Verlustrechnung je Buchungskreis, Mietobjekt und Zeitraum" :: Text)
               , "description_key" .= (moduleName <> ".module.description")
               , "objects" .= [object ["object" .= ("ProfitLoss" :: Text), "section" .= ("Auswertungen" :: Text), "section_key" .= (moduleName <> ".sections.auswertungen")]]
               ]
           ]
    , "translations" .= object [K.fromText loc .= object [K.fromText k .= v | (k, v) <- dict] | (loc, dict) <- translations]
    ]

translations :: [(Text, [(Text, Text)])]
translations =
  [ ("de", tr (\(d, _, _) -> d))
  , ("en", tr (\(_, e, _) -> e))
  , ("zh-CN", tr (\(_, _, z) -> z))
  ]
  where
    tr f = [(k, f v) | (k, v) <- texts]
    texts =
      [ (moduleName <> ".module.title", ("GuV", "P&L", "损益表"))
      , (moduleName <> ".module.description", ("Gewinn- und Verlustrechnung je Buchungskreis, Mietobjekt und Zeitraum", "Profit and loss statement by company code, rental object and period", "按公司代码、租赁对象和期间的损益表"))
      , (moduleName <> ".sections.auswertungen", ("Auswertungen", "Reports", "报表"))
      , (key "title", ("GuV-Rechnung", "Profit and loss statement", "损益表"))
      , (key "actions.compute", ("GuV berechnen", "Compute P&L", "计算损益"))
      , (key "fields.company_code", ("Buchungskreis", "Company code", "公司代码"))
      , (key "fields.rent_object_id", ("Mietobjekt (leer = alle)", "Rental object (empty = all)", "租赁对象（空 = 全部）"))
      , (key "fields.period_from", ("Zeitraum von", "Period from", "期间从"))
      , (key "fields.period_to", ("Zeitraum bis", "Period to", "期间至"))
      ]
