# ethiopianDate

ethiopianDate is an R package for converting Gregorian dates to the Ethiopian calendar and vice-versa.

## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install ethiopianDates. From the R command prompt:

```R
remotes::install_github('d-callan/ethiopianDate')
```

## Usage

```R
#year, month and date are all numeric values
gregorianToEthiopian(year, month, date)

#gregorianDate is a Date or character
#format (only used for character input) is the format of the input gregorianDate, default is '%Y-%m-%d'
toEthiopian(gregorianDate, format)
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)

## References

https://github.com/rgaudin/tools/tree/master/ethiopian_date

http://www.funaba.org/en/calendar-conversion.cgi

http://www.senamirmir.org/projects/
