package org.finos.symphony.messageml.messagemlutils.util.pojo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import org.finos.symphony.messageml.messagemlutils.elements.DatePicker;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;

/**
 * This class helps {@link DatePicker} to validate the content
 * of date intervals expressed in json format and also to convert them from MessageML format to PresentationML format:
 * basically, the json object for PresentationML has one more field: 'type', that can be calculated based on with fields are filled
 *
 * @author enrico.molino (18/11/2020)
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({ "type", "day", "from", "to", "daysOfWeek"})
public class DateInterval implements Serializable {

  private String day;
  private String from;
  private String to;
  private Integer[] daysOfWeek;

  @JsonIgnore
  private static final String DATE = "date";
  @JsonIgnore
  private static final String RANGE = "range";
  @JsonIgnore
  private static final String WEEKDAYS = "weekdays";

  public String getType(){
    if(onlyTheFirstNotNull(day, from, to, daysOfWeek)){
      return DATE;
    } else if(onlyTheFirstTwoNotNull(from, to, day, daysOfWeek)){
      return RANGE;
    } else if(onlyTheFirstNotNull(daysOfWeek, from, to, day)){
      return WEEKDAYS;
    }
    return null;
  }

  @JsonIgnore
  public void assertIsValid() throws InvalidInputException {
    String type = getType();
    if(DATE.equals(type)){
      assertDateFormat(day);
    } else if(RANGE.equals(type)){
      assertDateFormat(from);
      assertDateFormat(to);
    } else if(WEEKDAYS.equals(type)){
      if(Arrays.stream(daysOfWeek).anyMatch(d -> d < 0 || d > 6)){
        throw new InvalidInputException("'daysOfWeek' out of range [0-6]");
      }
    } else {
      throw new InvalidInputException("Date interval 'type' is unknown or null");
    }
  }

  public String getDay() {
    return day;
  }

  public void setDay(String day) {
    this.day = day;
  }

  public String getFrom() {
    return from;
  }

  public void setFrom(String from) {
    this.from = from;
  }

  public String getTo() {
    return to;
  }

  public void setTo(String to) {
    this.to = to;
  }

  public Integer[] getDaysOfWeek() {
    return daysOfWeek;
  }

  public void setDaysOfWeek(Integer[] daysOfWeek) {
    this.daysOfWeek = daysOfWeek;
  }

  private boolean onlyTheFirstNotNull(Object notNull, Object... nulls){
    if(notNull == null){
      return false;
    }
    for(Object n: nulls){
      if(n != null){
        return false;
      }
    }
    return true;
  }

  private boolean onlyTheFirstTwoNotNull(Object notNull1, Object notNull2, Object... nulls){
    if(notNull1 == null){
      return false;
    }
    return onlyTheFirstNotNull(notNull2, nulls);
  }

  private void assertDateFormat(String date) throws
      InvalidInputException {
    try {
      LocalDate.parse(date, DateTimeFormatter.ISO_DATE);
    } catch (DateTimeParseException e) {
      throw new InvalidInputException(String.format("\"%s\" is not a valid date in ISO_8601 format", date), e);
    }
  }
}
