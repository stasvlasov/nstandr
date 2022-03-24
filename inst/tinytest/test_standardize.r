## -------->>  [[file:../../harmonizer.src.org::*standardize][standardize:2]]
dummy <- function(x, n) {
    for(i in 1:n) x <- sqrt(x)^2
    return(x)
}

## note that do.call is not able to find functions by name so one need to pass it as a lambda

expect_equal(
    standardize(as.numeric(1:10^2)
            , procedures =
                  list("Squaring stuff" = "sqrt"
                      ,list("abs", show_progress = FALSE)
                      ,list("log", base = 10)
                     , "My function" = list(dummy, 10^6, show_progress = TRUE))
            , nrows_min_to_show_progress = 1
            , progress_step_nrows = NULL
            , quite = TRUE)
, c(0, 0.150514997831991, 0.238560627359831, 0.301029995663981, 
      0.349485002168009, 0.389075625191822, 0.422549020007129, 0.451544993495972, 
      0.477121254719662, 0.5, 0.520696342579113, 0.539590623023813, 
      0.556971676153418, 0.573064017839119, 0.588045629527841, 0.602059991327962, 
      0.615224460689137, 0.627636252551653, 0.639376800476415, 0.650514997831991, 
      0.66110964736696, 0.671211340411103, 0.680863918008797, 0.690105620855803, 
      0.698970004336019, 0.707486673985409, 0.715681882079494, 0.72357901567111, 
      0.731198998949478, 0.738560627359831, 0.745680846917136, 0.752574989159953, 
      0.759256969938944, 0.765739458521128, 0.772034022175138, 0.778151250383644, 
      0.784100862033497, 0.789891798308405, 0.79553230351325, 0.801029995663981, 
      0.806391928359868, 0.81162464519895, 0.816734227789793, 0.821726338243094, 
      0.826606256887672, 0.831378915840787, 0.836048928967859, 0.840620618687793, 
      0.845098040014257, 0.849485002168009, 0.853785088048968, 0.8580016718174, 
      0.862137934800395, 0.866196879911484, 0.870181344747122, 0.8740940135031, 
      0.877937427836246, 0.881713996781469, 0.885426005821072, 0.889075625191822, 
      0.892664917505384, 0.896195844749127, 0.899670274726791, 0.903089986991944, 
      0.906456678321428, 0.909771967770934, 0.913037401350413, 0.916254456353118, 
      0.919424545368627, 0.922549020007128, 0.925629174359538, 0.928666248215634, 
      0.931661430060228, 0.934615859865488, 0.93753063169585, 0.940406796140396, 
      0.943245362586241, 0.94604730134524, 0.948813545645221, 0.951544993495972, 
      0.954242509439325, 0.956906926191858, 0.959539046188037, 0.962139643030941, 
      0.964709462857146, 0.967249225621784, 0.969759626309309, 0.972241336075084, 
      0.974695003322457, 0.977121254719662, 0.979520696160547, 0.981893913672778, 
      0.984241474276968, 0.986563926799849, 0.988861802644424, 0.991135616519784, 
      0.993385867133122, 0.995613037846247, 0.997817597298775, 1))



expect_equal(c("žŸong-ÂÃÇÈÏa\n\u00b5 &oacute;\u00b5<p>,  INt LTD &AMP; Co;  "
             , "<br> the $ (&AMP; C&oacute;MP comPANY) Ïotta"
             , "Tempshield Cryo-Protection™"
             , "Ábcdêãçoàúü"
             , "Polgen Sp. z o.o. <U+0096> Sp. K."
             , "Polgen Sp. z o.o. – Sp. K."
             , "Jerome® <br>"
             , "Controlled Environments®  Magazine"
             , "a\n\u00b5\u00b5"
             , "fa\xE7ile"
             , "fa\xc3\xa7ile"
             , "MSlab CO. CO., LTD."
             , "MSlab, A \\SOCIETE ANONYME\\"
             , "S.A.S. University Co., {PE}, Ltd. (Europe)"
             , "Analytical Technologies Limited"
             , "Anasys Instruments Corporation"
             , "C4 Control de Contaminacion Ltda"
             , "Crescent Scientific Pvt Ltd."
             , "Daigger & Co., Inc."
             , "Dell Inc."
             , "Deltalab. S.L.U."
             , "DLAB Scientific Co.,Ltd."
             , "ebro Electronic GmbH und Co. KG"
             , "Ecom spol. s r.o., s.r.o., akc. spol."
             , "G.A.S. mbH"
             , "Glassco Laboratory Equipments PVT LTD"
             , "Lhasa Limited"
             , "rose plastic USA, LLLP"
             , "a;sdkfjsdlkfj;laswee\'\" asdf Co.") |>
             standardize(quite = TRUE)
           , c("ZYONG AACEIA U OU INT LTD & CO", "THE & COMP COMPANY IOTTA", 
               "TEMPSHIELD CRYO PROTECTION", "ABCDEACOAUU", "POLGEN SP Z OO SP K", 
               "POLGEN SP Z OO SP K", "JEROME", "CONTROLLED ENVIRONMENTS MAGAZINE", 
               "A UU", "FAILE", "FACILE", "MSLAB CO CO LTD", "MSLAB A SOCIETE ANONYME", 
               "SAS UNIVERSITY CO PE LTD EUROPE", "ANALYTICAL TECHNOLOGIES LIMITED", 
               "ANASYS INSTRUMENTS CORPORATION", "C4 CONTROL DE CONTAMINACION LTDA", 
               "CRESCENT SCIENTIFIC PVT LTD", "DAIGGER & CO INC", "DELL INC", 
               "DELTALAB SLU", "DLAB SCIENTIFIC COLTD", "EBRO ELECTRONIC GMBH & CO KG", 
               "ECOM SPOL S RO SRO AKC SPOL", "GAS MBH", "GLASSCO LABORATORY EQUIPMENTS PVT LTD", 
               "LHASA LIMITED", "ROSE PLASTIC USA LLLP", "ASDKFJSDLKFJLASWEE\" ASDF CO"
               ))
## --------<<  standardize:2 ends here


