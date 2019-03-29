Hour
~~~
getHour
~~~
getDay
~~~
getYear
~~~
getMonth
~~~
getDayOfMonth
~~~
getFirstMillisecond
~~~
getLastMillisecond
~~~
peg
~~~
previous
~~~
next
~~~
getSerialIndex
~~~
getFirstMillisecond
~~~
getLastMillisecond
~~~
equals
~
if (obj == this) {
    return true;
}
~
if (!(obj instanceof Hour)) {
    return false;
}
~
Hour that = (Hour) obj;
~
if (this.hour != that.hour) {
    return false;
}
~
if (!this.day.equals(that.day)) {
    return false;
}
~~~
toString
~~~
hashCode
~
result = 37 * result + this.hour;
~
result = 37 * result + this.day.hashCode();
~~~
compareTo
~
result = this.hour - h.getHour();
~
// more difficult case - evaluate later...
result = 0;
~
// consider time periods to be ordered after general objects
result = 1;
~
if (result == 0) {
    result = this.hour - h.getHour();
}
~~~
parseHour
~
hourstr = hourstr.trim();
~
s = s.trim();
