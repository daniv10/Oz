/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Find the lowest temperature in any number of files of CSV weather data chosen by the user *
  * * * * * * * * * * * * * * * * Credit for Duke Software Team * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
                                   
import edu.duke.*;
import org.apache.commons.csv.*;
import java.io.*;

public class findColdestDay {
	public CSVRecord hottestHourInFile(CSVParser parser) {
		//start with lowestSoFar as nothing
		CSVRecord lowestSoFar = null;
		//For each row (currentRow) in the CSV File
		for (CSVRecord currentRow : parser) {
			//If lowestSoFar is nothing
			if (lowestSoFar == null) {
				lowestSoFar = currentRow;
			}
			//Otherwise
			else {
				double currentTemp = Double.parseDouble(currentRow.get("TemperatureF"));
				double lowestTemp = Double.parseDouble(lowestSoFar.get("TemperatureF"));
				//Check if currentRow’s temperature > lowestSoFar’s
				if (currentTemp < lowestTemp) {
					//If so update lowestSoFar to currentRow
					lowestSoFar = currentRow;
				}
			}
		}
		//The lowestSoFar is the answer
		return lowestSoFar;
	}

	public CSVRecord hottestInManyDays() {
		CSVRecord lowestSoFar = null;
		DirectoryResource dr = new DirectoryResource();
		// iterate over files
		for (File f : dr.selectedFiles()) {
			FileResource fr = new FileResource(f);
			// use method to get lowest in file.
			CSVRecord currentRow = hottestHourInFile(fr.getCSVParser());
			if (lowestSoFar == null) {
				lowestSoFar = currentRow;
			}
			//Otherwise
			else {
				double currentTemp = Double.parseDouble(currentRow.get("TemperatureF"));
				double lowestTemp = Double.parseDouble(lowestSoFar.get("TemperatureF"));
				//Check if currentRow’s temperature is greater than lowestSoFar’s
				if (currentTemp < lowestTemp) {
					//If so update lowestSoFar to currentRow
					lowestSoFar = currentRow;
				}
			}
		}
		//The lowestSoFar is the answer
		return lowestSoFar;
	}
	
	public void testHottestInManyDays () {
		CSVRecord lowest = hottestInManyDays();
		System.out.println("The coldest temperature was " + lowest.get("TemperatureF") +
				   " at " + lowest.get("DateUTC"));
	}
}
