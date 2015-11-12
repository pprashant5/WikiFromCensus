object DistrictPages extends App {
   //def rawfile = args(0)
   //def disctrictFile = if (rawfile.endsWith(".csv")) rawfile else rawfile+".csv"
 
   
   val district = new WikiFromCensus(args(0), args(1))
   district.saveAll
}