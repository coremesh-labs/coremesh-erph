-- | Metamodell (Catalog.Describe): Modul GuV mit den Auswertungen ProfitLoss
-- (GuV), TrialBalance (Summen- und Saldenliste) und AccountSheet (Kontoblatt) –
-- Objects ohne Übersicht; der WebServer zeigt die Maske der Aktion compute,
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

key :: Text -> Text -> Text
key obj k = moduleName <> "." <> obj <> "." <> k

lookupOf :: Text -> Text -> [Text] -> [(Text, Text)] -> Maybe Value
lookupOf obj value labels filters =
  Just (object (["object" .= obj, "value_field" .= value, "label_fields" .= labels] <> ["filters" .= object [K.fromText k .= v | (k, v) <- filters] | not (null filters)]))

-- | Feld: (Schlüssel, Typ, Pflicht, Lookup, Auswahl, Texte de/en/zh)
data Field = Field Text Text Bool (Maybe Value) [(Text, (Text, Text, Text))] (Text, Text, Text)

companyCode, periodFrom, periodTo, fiscalYear, building, entity :: Field
companyCode = Field "company_code" "text" True (lookupOf "CompanyCode" "code" ["description"] []) [] ("Buchungskreis", "Company code", "公司代码")
periodFrom = Field "period_from" "date" True Nothing [] ("Zeitraum von", "Period from", "期间从")
periodTo = Field "period_to" "date" True Nothing [] ("Zeitraum bis", "Period to", "期间至")
fiscalYear = Field "fiscal_year" "number" True Nothing [] ("Geschäftsjahr", "Fiscal year", "会计年度")
building =
  Field "building_id" "text" False (lookupOf "Building" "building_id" ["designation"] [("company_code", "company_code")]) []
    ("Gebäude (leer = alle)", "Building (empty = all)", "建筑（空 = 全部）")
entity =
  Field "entity_id" "text" False (lookupOf "BusinessEntity" "entity_id" ["designation"] [("company_code", "company_code")]) []
    ("Wirtschaftseinheit (leer = alle)", "Business entity (empty = all)", "经济单位（空 = 全部）")

-- | (Object, Icon, Titel, Aktion, Felder)
reports :: [(Text, Text, (Text, Text, Text), (Text, Text, Text), [Field])]
reports =
  [ ( "ProfitLoss"
    , "icon-chart"
    , ("GuV-Rechnung", "Profit and loss statement", "损益表")
    , ("GuV berechnen", "Compute P&L", "计算损益")
    , [ companyCode
      , entity
      , building
      , Field "rent_object_id" "text" False (lookupOf "RentObject" "object_id" ["designation"] [("company_code", "company_code")]) []
          ("Mietobjekt (leer = alle)", "Rental object (empty = all)", "租赁对象（空 = 全部）")
      , Field "group_by" "select" False Nothing
          [ ("BUILDING", ("je Gebäude", "per building", "按建筑"))
          , ("ENTITY", ("je Wirtschaftseinheit", "per business entity", "按经济单位"))
          ]
          ("Spalten (leer = nur Summe)", "Columns (empty = total only)", "列（空 = 仅合计）")
      , periodFrom
      , periodTo
      ]
    )
  , ( "TrialBalance"
    , "icon-list"
    , ("Summen- und Saldenliste", "Trial balance", "试算平衡表")
    , ("Saldenliste anzeigen", "Show trial balance", "显示试算平衡表")
    , [ companyCode
      , entity
      , building
      , fiscalYear
      , periodFrom
      , periodTo
      , Field "account_from" "text" False (lookupOf "GLAccountCompany" "account_number" ["account_name"] [("company_code_id", "company_code")]) []
          ("Konto von", "Account from", "科目从")
      , Field "account_to" "text" False (lookupOf "GLAccountCompany" "account_number" ["account_name"] [("company_code_id", "company_code")]) []
          ("Konto bis", "Account to", "科目至")
      , Field "only_open" "boolean" False Nothing [] ("Nur nicht ausgeglichene Konten", "Only accounts with a balance", "仅显示未结平科目")
      ]
    )
  , ( "AccountSheet"
    , "icon-book"
    , ("Kontoblatt", "Account sheet", "科目明细账")
    , ("Kontoblatt anzeigen", "Show account sheet", "显示科目明细账")
    , [ companyCode
      , Field "account_number" "text" True (lookupOf "GLAccountCompany" "account_number" ["account_name"] [("company_code_id", "company_code")]) []
          ("Sachkonto", "G/L account", "总账科目")
      , entity
      , building
      , fiscalYear
      , periodFrom
      , periodTo
      , Field "contract_id" "text" False (lookupOf "Contract" "contract_id" ["designation"] [("company_code", "company_code")]) []
          ("Vertrag (leer = alle)", "Contract (empty = all)", "合同（空 = 全部）")
      , Field "partner_id" "text" False (lookupOf "BusinessPartner" "id" ["search_term", "name1"] []) []
          ("Geschäftspartner (leer = alle)", "Business partner (empty = all)", "业务伙伴（空 = 全部）")
      ]
    )
  ]

describe :: Value
describe =
  object
    [ "objects"
        .= [ object
               [ "name" .= obj
               , "title" .= de title
               , "title_key" .= key obj "title"
               , "icon" .= icon
               , "form_state" .= ("formState" :: Text)
               , "lifecycle" .= object ["type" .= ("immutable" :: Text)]
               , "fields" .= map (fieldJSON obj) fields
               , "actions"
                   .= [ object
                          [ "name" .= ("compute" :: Text)
                          , "kind" .= ("custom" :: Text)
                          , "label" .= de action
                          , "label_key" .= key obj "actions.compute"
                          , "fields" .= [k | Field k _ _ _ _ _ <- fields]
                          , "form_state" .= True
                          ]
                      ]
               ]
           | (obj, icon, title, action, fields) <- reports
           ]
    , "modules"
        .= [ object
               [ "name" .= moduleName
               , "title" .= ("GuV" :: Text)
               , "title_key" .= (moduleName <> ".module.title")
               , "icon" .= ("icon-chart" :: Text)
               , "description" .= ("Auswertungen des Hauptbuchs: GuV je Gebäude, Saldenliste, Kontoblatt" :: Text)
               , "description_key" .= (moduleName <> ".module.description")
               , "objects"
                   .= [ object ["object" .= obj, "section" .= ("Auswertungen" :: Text), "section_key" .= (moduleName <> ".sections.auswertungen")]
                      | (obj, _, _, _, _) <- reports
                      ]
               ]
           ]
    , "translations" .= object [K.fromText loc .= object [K.fromText k .= v | (k, v) <- dict] | (loc, dict) <- translations]
    ]
  where
    de (d, _, _) = d :: Text

fieldJSON :: Text -> Field -> Value
fieldJSON obj (Field k t r lk opts (l, _, _)) =
  object
    ( [ "key" .= k
      , "label" .= l
      , "label_key" .= key obj ("fields." <> k)
      , "type" .= t
      , "required" .= r
      , "editable" .= True
      , "listable" .= False
      , "action_only" .= True
      ]
        <> maybe [] (\v -> ["lookup" .= v]) lk
        <> [ "options" .= [object ["value" .= v, "label" .= d, "label_key" .= key obj ("options." <> k <> "." <> optKey v)] | (v, (d, _, _)) <- opts]
           | not (null opts)
           ]
    )

optKey :: Text -> Text
optKey = id

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
      , (moduleName <> ".module.description", ("Auswertungen des Hauptbuchs: GuV je Gebäude, Saldenliste, Kontoblatt", "General ledger reports: P&L by building, trial balance, account sheet", "总账报表：按建筑的损益表、试算平衡表、科目明细账"))
      , (moduleName <> ".sections.auswertungen", ("Auswertungen", "Reports", "报表"))
      ]
        <> concat
          [ [(key obj "title", title), (key obj "actions.compute", action)]
              <> [(key obj ("fields." <> k), l) | Field k _ _ _ _ l <- fields]
              <> [(key obj ("options." <> k <> "." <> optKey v), o) | Field k _ _ _ opts _ <- fields, (v, o) <- opts]
          | (obj, _, title, action, fields) <- reports
          ]
