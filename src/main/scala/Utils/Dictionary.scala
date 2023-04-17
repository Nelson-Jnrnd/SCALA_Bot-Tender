package Utils

/**
* Contains the dictionary of the application, which is used to validate, correct and normalize words entered by the
* user.
*/
object Dictionary:
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",

    "je" -> "je",
    "j" -> "je",

    "suis" -> "etre",
    "est" -> "etre",

    "veux" -> "vouloir",
    "aimerais" -> "vouloir",
    "voudrais" -> "vouloir",

    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",

    "affamé" -> "affame",
    "affamée" -> "affame",

    "bière" -> "biere",
    "bières" -> "biere",

    "croissant" -> "croissant",
    "croissants" -> "croissant",

    "et" -> "et",

    "ou" -> "ou",

    "svp" -> "svp",
    "stp" -> "svp",
  
    "maison" -> "maison",
    "cailler" -> "cailler",
    "farmer" -> "farmer",
    "boxer" -> "boxer",
    "wittekop" -> "wittekop",
    "punkipa" -> "punkipa",
    "jackhammer" -> "jackhammer",
    "ténébreuse" -> "tenebreuse",

    "me" -> "me",
    "m'" -> "me",

    "appelle" -> "appeler",
    
    "commander" -> "commander",
    
    "connaître" -> "connaitre",

    "mon" -> "mon",
    "ma" -> "mon", // maybe remove
    "mes" -> "mon", // maybe remove

    "solde" -> "solde",

    "combien" -> "combien",

    "coûte" -> "couter",
    "coute" -> "couter",
    "coutent" -> "couter",
    "coûtent" -> "couter",

    "quel" -> "quel",

    "le" -> "le",

    "prix" -> "prix",

    "de" -> "de",
    "des" -> "de",
    "d'" -> "de",
    "du" -> "de",
  )
end Dictionary
