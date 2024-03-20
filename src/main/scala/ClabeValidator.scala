object ClabeValidator {
  val BANKS: Map[String, String] = Map(
    "002" -> "BANAMEX",
    "006" -> "BANCOMEXT",
    "009" -> "BANOBRAS",
    "012" -> "BBVA BANCOMER",
    "014" -> "SANTANDER",
    "019" -> "BANJERCITO",
    "021" -> "HSBC",
    "030" -> "BAJIO",
    "032" -> "IXE",
    "036" -> "INBURSA",
    "037" -> "INTERACCIONES",
    "042" -> "MIFEL",
    "044" -> "SCOTIABANK",
    "058" -> "BANREGIO",
    "059" -> "INVEX",
    "060" -> "BANSI",
    "062" -> "AFIRME",
    "072" -> "BANORTE",
    "102" -> "THE ROYAL BANK",
    "103" -> "AMERICAN EXPRESS",
    "106" -> "BAMSA",
    "108" -> "TOKYO",
    "110" -> "JP MORGAN",
    "112" -> "BMONEX",
    "113" -> "VE POR MAS",
    "116" -> "ING",
    "124" -> "DEUTSCHE",
    "126" -> "CREDIT SUISSE",
    "127" -> "AZTECA",
    "128" -> "AUTOFIN",
    "129" -> "BARCLAYS",
    "130" -> "COMPARTAMOS",
    "131" -> "BANCO FAMSA",
    "132" -> "BMULTIVA",
    "133" -> "ACTINVER",
    "134" -> "WAL-MART",
    "135" -> "NAFIN",
    "136" -> "INTERBANCO",
    "137" -> "BANCOPPEL",
    "138" -> "ABC CAPITAL",
    "139" -> "UBS BANK",
    "140" -> "CONSUBANCO",
    "141" -> "VOLKSWAGEN",
    "143" -> "CIBANCO",
    "166" -> "BANSEFI",
    "168" -> "HIPOTECARIA FEDERAL",
    "600" -> "MONEXCB",
    "601" -> "GBM",
    "602" -> "MASARI",
    "605" -> "VALUE",
    "606" -> "ESTRUCTURADORES",
    "607" -> "TIBER",
    "608" -> "VECTOR",
    "610" -> "B&B",
    "614" -> "ACCIVAL",
    "615" -> "MERRILL LYNCH",
    "616" -> "FINAMEX",
    "617" -> "VALMEX",
    "618" -> "UNICA",
    "619" -> "MAPFRE",
    "620" -> "PROFUTURO",
    "621" -> "CB ACTINVER",
    "622" -> "OACTIN",
    "623" -> "SKANDIA",
    "626" -> "CBDEUTSCHE",
    "627" -> "ZURICH",
    "628" -> "ZURICHVI",
    "629" -> "SU CASITA",
    "630" -> "CB INTERCAM",
    "631" -> "CI BOLSA",
    "632" -> "BULLTICK CB",
    "633" -> "STERLING",
    "634" -> "FINCOMUN",
    "636" -> "HDI SEGUROS",
    "637" -> "ORDER",
    "638" -> "NU MEXICO",
    "640" -> "CB JPMORGAN",
    "642" -> "REFORMA",
    "646" -> "STP",
    "647" -> "TELECOMM",
    "648" -> "EVERCORE",
    "649" -> "SKANDIA",
    "651" -> "SEGMTY",
    "652" -> "ASEA",
    "653" -> "KUSPIT",
    "655" -> "SOFIEXPRESS",
    "656" -> "UNAGRA",
    "659" -> "OPCIONES EMPRESARIALES DEL NOROESTE",
    "901" -> "CLS",
    "902" -> "INDEVAL SD",
    "670" -> "LIBERTAD"
  )

  val CLABE_LENGTH: Int = 18
  val CLABE_WEIGHTS: List[Int] = List(3, 7, 1, 3, 7, 1, 3, 7, 1, 3, 7, 1, 3, 7, 1, 3, 7)

  def computeControlDigit(clabe: String): String = {
    /*
    Compute CLABE control digit according to
      https://es.wikipedia.org/wiki/CLABE#D.C3.ADgito_control
    */
    val clabeList: List[Char] = clabe.toList
    val clabeInt: List[Int] = clabeList.map(_.toString.toInt)
    val weighted: List[Int] = (0 until CLABE_LENGTH - 1).map(i => clabeInt(i) * CLABE_WEIGHTS(i) % 10).toList
    val summed: Int = weighted.sum % 10
    val controlDigit: Int = (10 - summed) % 10
    controlDigit.toString
  }

  def validateClabe(clabe: String): Boolean = {
    /*
    Validate CLABE according to
      https://es.wikipedia.org/wiki/CLABE#D.C3.ADgito_control
    */
    isANumber(clabe) &&
      clabe.length == CLABE_LENGTH &&
      BANKS.contains(clabe.substring(0, 3)) &&
      clabe.substring(CLABE_LENGTH - 1) == computeControlDigit(clabe)
  }

  def getBankName(clabe: String): String = {
    /*
    Returns the bank name based on the first 3 digits
      https://es.wikipedia.org/wiki/CLABE#D.C3.ADgito_control
    */
    val code: String = clabe.substring(0, 3)
    val bankName: Option[String] = BANKS.get(code)
    bankName.getOrElse(throw new Exception(s"No bank has this code: $code"))
  }

  // Will return true only if characters in a string are digits
  def isANumber(str: String): Boolean = str.forall(_.isDigit)
}

// Usage:
// ClabeValidator.validateClabe("123456789012345678") // Validate CLABE
// ClabeValidator.getBankName("123456789012345678") // Get bank name
// ClabeValidator.computeControlDigit("123456789012345678") // Compute control digit
