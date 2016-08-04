package chap01;

public class HelloWorldJava {
  static String greeting1 = "Hello, world!"; //#A
  static String greeting2 = "Howdy, universe!"; //#A
  static String greeting3 = "Oh no, not again"; //#A
  
  static double pSunnyToday = 0.2; //#B
  static double pNotSunnyToday = 0.8; //#B
  static double pSunnyTomorrowIfSunnyToday = 0.8; //#B
  static double pNotSunnyTomorrowIfSunnyToday = 0.2; //#B
  static double pSunnyTomorrowIfNotSunnyToday = 0.05; //#B
  static double pNotSunnyTomorrowIfNotSunnyToday = 0.95; //#B
  static double pGreeting1TodayIfSunnyToday = 0.6; //#B
  static double pGreeting2TodayIfSunnyToday = 0.4; //#B
  static double pGreeting1TodayIfNotSunnyToday = 0.2; //#B
  static double pGreeting3IfNotSunnyToday = 0.8; //#B
  static double pGreeting1TomorrowIfSunnyTomorrow = 0.5; //#B
  static double pGreeting2TomorrowIfSunnyTomorrow = 0.5; //#B
  static double pGreeting1TomorrowIfNotSunnyTomorrow = 0.1; //#B
  static double pGreeting3TomorrowIfNotSunnyTomorrow = 0.95; //#B
  
  static void predict() { //#C
	  double pGreeting1Today = //#C
		pSunnyToday * pGreeting1TodayIfSunnyToday + //#C
		pNotSunnyToday * pGreeting1TodayIfNotSunnyToday; //#C
	System.out.println("Today's greeting is " + greeting1 + //#C
	                   "with probability " + pGreeting1Today + "."); //#C
  } //#C
  
  static void infer() { //#D
	  double pSunnyTodayAndGreeting1Today = //#D
		pSunnyToday * pGreeting1TodayIfSunnyToday; //#D
	  double pNotSunnyTodayAndGreeting1Today = //#D
		pNotSunnyToday * pGreeting1TodayIfNotSunnyToday; //#D
	  double pSunnyTodayGivenGreeting1Today = //#D
		pSunnyTodayAndGreeting1Today / //#D
		(pSunnyTodayAndGreeting1Today + pNotSunnyTodayAndGreeting1Today); //#D
	System.out.println("If today's greeting is " + greeting1 + //#D
					   ", today's weather is sunny with probability " + //#D
					   pSunnyTodayGivenGreeting1Today + "."); //#D
  } //#D
  
  static void learnAndPredict() { //#E
	  double pSunnyTodayAndGreeting1Today = //#E
		pSunnyToday * pGreeting1TodayIfSunnyToday; //#E
	  double pNotSunnyTodayAndGreeting1Today = //#E
		pNotSunnyToday * pGreeting1TodayIfNotSunnyToday; //#E
	  double pSunnyTodayGivenGreeting1Today = //#E
		pSunnyTodayAndGreeting1Today / //#E
		(pSunnyTodayAndGreeting1Today + pNotSunnyTodayAndGreeting1Today); //#E
	  double pNotSunnyTodayGivenGreeting1Today = //#E
		1 - pSunnyTodayGivenGreeting1Today; //#E
	  double pSunnyTomorrowGivenGreeting1Today = //#E
		pSunnyTodayGivenGreeting1Today * pSunnyTomorrowIfSunnyToday + //#E
		pNotSunnyTodayGivenGreeting1Today * pSunnyTomorrowIfNotSunnyToday; //#E
	  double pNotSunnyTomorrowGivenGreeting1Today = //#E
		1 - pSunnyTomorrowGivenGreeting1Today; //#E
	  double pGreeting1TomorrowGivenGreeting1Today = //#E
		pSunnyTomorrowGivenGreeting1Today * pGreeting1TomorrowIfSunnyTomorrow + //#E
		pNotSunnyTomorrowGivenGreeting1Today * pGreeting1TomorrowIfNotSunnyTomorrow; //#E
	System.out.println("If today's greeting is " + greeting1 + //#E
					   ", tomorrow's greeting will be " + greeting1 + //#E
					   " with probability " + //#E
					   pGreeting1TomorrowGivenGreeting1Today); //#E
  } //#E
  
  public static void main(String[] args) { //#F
    predict(); //#F
	infer(); //#F
	learnAndPredict(); //#F
  } //#F
  
}