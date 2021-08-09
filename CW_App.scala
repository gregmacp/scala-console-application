  import scala.io.Source
  import scala.io.StdIn.readInt
  import scala.io.StdIn.readLine
  import scala.collection.immutable.ListMap



  object CW_App extends App {
    // *******************************************************************************************************************
    // APPLICATION LOGIC
    val citiesMap = readFile("weatherdata14March.txt")
    for ((city, v) <- citiesMap) {
      println(city + " - " + v)
    }

    // *******************************************************************************************************************
    // UTILITY FUNCTIONS

    // reads data file - comma separated file
    def readFile(filename: String): Map[String, List[(Int, Int)]] = {
      def stringToTuple(s: String): (Int, Int) = {
        val Array(a, b) = s.split(":").map(_.trim.toInt)
        //              println("this is to see if they are ints **** " + a + " + " + b + " = " + (a + b))
        (a, b)
      }

      var citiesMap: Map[String, List[(Int, Int)]] = Map()
      try {
        for (line <- Source.fromFile(filename).getLines()) { // for each line
          val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List
          citiesMap = citiesMap ++ Map(splitline.head -> (for (x <- splitline.tail.toList) yield stringToTuple(x)))
        }

      } catch {
        case ex: Exception => println("Sorry, an exception happened.")
      }
      citiesMap
    }

    // define menu options as a Map of actions
    // for each menu item:
    // key is an Int, the value that will be read from the input
    // value is a function () => Boolean, i.e. no params and returns Boolean
    val actionMap = Map[Int, () => Boolean](1 -> handleOne, 3 -> handleThree, 4 -> handleFour, 5 -> handleFive, 6 -> handleSix, 7 -> handleSeven, 8 -> handleEight)

    var opt = 0
    do {
      opt = readOption
      println(opt)
    } while (menu(opt))

    //
    //    // *******************************************************************************************************************
    //    // FUNCTIONS FOR MENU
    //
    //    shows menu and reads input
    def readOption: Int = {
      println(
        """|Please select one of the following:
           |  1 - say Hello
           |  3 - get most recent temperature
           |  4 - differences between min / max
           |  5 - average difference
           |  6 - greatest difference
           |  7 - construct an itinerary
           |  8 - quit""".stripMargin)
      readInt()
    }

    // invokes selected menu option
    // finds corresponding function to invoke in action map using get
    // pattern matching used as get returns an Option
    def menu(option: Int): Boolean = {
      actionMap.get(option) match {
        case Some(f) => f()
        case None =>
          println("Sorry, that command is not recognized")
          true
      }
    }


    //
    //    // handlers for menu options
    def handleOne(): Boolean = {
      println("Hello!")
      true
    }

    //

    def handleThree(): Boolean = {
      println("Here are last year's min and max temperatures")
      for ((city, v) <- citiesMap) {
        println(city + " - " + mostRecent(v))
      }
      true
    }

    def handleFour(): Boolean = {
      println("Here are the differences between the min and max temperatures for each year")
      for ((city, v) <- citiesMap) {
        println(city + " - " + (difference(v)))
      }
      true
    }

    def handleFive(): Boolean = {
      //      reuse function for max min temperature
      println("Here is the average difference between the min and max temperatures for each city")
      //      print(average(differences(citiesMap)))
      for ((city, v) <- citiesMap) {
        println(city + " - " + average(difference(v)))
      }
      true
    }

    def handleSix(): Boolean = {
      //    Sevilla -> 20
      //    Salamanca -> 18
      println("here is the greatest difference in min and max temperatures for each city")
      for ((city, v) <- citiesMap) {
        println(city + " - " + difference(v).max)
      }
      true
    }

    def handleSeven(): Boolean = {
      println("**Building Itinerary ")
      println("**Getting cities...")
      summary(selectCities().filter(confirmCity(_)))
      println("******************************")
      true
    }

    def handleEight(): Boolean = {
      println("selected quit") // returns false so loop terminates
      false
    }


    //
    // Operation handlers
    //
    def summary(confirmedNames:List[String]): Unit = {
      //gets user input and checks for valid inputs
      println(confirmedNames)
      println("**************************************")
      println("************ Trip Summary ************")
      println("**************************************")
      println("** Most Recent Min/Max Temperatures **")
      println("**************************************")

      val destinationsMap = citiesMap.filterKeys(confirmedNames.contains(_))

      for ((city, v) <- destinationsMap) {
        println("- " + city + " - " + mostRecent(v))
      }

      println("******************************")

      val listOfMins = (for ((city, v) <- destinationsMap)
        yield mostRecent(v)._1).toList
      val listOfMaxes = (for ((city, v) <- destinationsMap)
        yield mostRecent(v)._2).toList

      println("The overall average minimum/maximum temperatures for this itinerary are: " + overallAv(listOfMins,listOfMaxes))
    }

    def overallAv(listOfMins:List[Int],listOfMaxes:List[Int]): (Double,Double)={
      (average(listOfMins),average(listOfMaxes))
    }

//    Takes in a string and returns true if this string is one of the cities inside our data set
    def confirmCity(s: String): Boolean = {
      if (citiesMap.keys.toList.contains(s)) true
      else false
    }

//
//
//    Functions
//
//
//    Takes list of Ints, converts to a list of Doubles and then calculates the average, then formats this to a single precision Double
    def average(differences: List[Int]): Double = {
      format(differences.map(x => x.toDouble).foldLeft(0.0)(_ + _) / differences.length)
    }

//    Formats a double to a more readable precision (eg. 7.6666666 -> 7.7)
    def format(d: Double): Double = {
      BigDecimal(d).setScale(1, BigDecimal.RoundingMode.HALF_DOWN).toDouble
    }

//    Takes List of tuples and returns a list of ints showing the difference bt max and min
    def difference(data: List[(Int, Int)]): List[Int] = {
      for (tuple <- data)
        yield tuple._2 - tuple._1
    }

//    Takes string value and returns tuple with most recent data
    def mostRecent(value: List[(Int, Int)]): (Int, Int) = {
      value.head
    }

//    Takes user input and comma-separates to return a list of Strings
    def selectCities(): List[String] = {
      println("Please specify cities, separated by commas eg: Madrid, Granada, Bilbao")
      readLine().split(",").map(_.trim).toList
    }

//    Old way of calculating average without formatting
//    def oldAverage(differences: List[Int]): Double = {
//      differences.foldLeft(1)((q, w) => q + w) / differences.length
//    }

  }

