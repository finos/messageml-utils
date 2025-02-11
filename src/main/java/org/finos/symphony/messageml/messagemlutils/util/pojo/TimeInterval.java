package org.finos.symphony.messageml.messagemlutils.util.pojo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import org.finos.symphony.messageml.messagemlutils.elements.TimePicker;
import org.finos.symphony.messageml.messagemlutils.exceptions.InvalidInputException;

import java.io.Serializable;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * This class helps {@link TimePicker} to validate the content
 * of time intervals expressed in json format and also to convert them from MessageML format to PresentationML format:
 * basically, the json object for PresentationML has one more field: 'type', that can be calculated based on with fields are filled
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({"type", "time", "from", "to"})
public class TimeInterval implements Serializable {

  private String time;
  private String from;
  private String to;

  @JsonIgnore
  private static final String TIME = "time";

  @JsonIgnore
  private static final String RANGE = "range";

  public String getTime() {
    return time;
  }

  public void setTime(String time) {
    this.time = time;
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


  @JsonIgnore
  public void assertIsValid() throws InvalidInputException {
    String type = getType();
    if (TIME.equals(type)) {
      assertTimeFormat(time);
    } else if (RANGE.equals(type)) {
      assertTimeFormat(from);
      assertTimeFormat(to);
    } else {
      throw new InvalidInputException("Time interval 'type' is unknown or null");
    }
  }

  public String getType() {
    if (time != null && from == null && to == null) {
      return TIME;
    } else if (time == null && from != null && to != null) {
      return RANGE;
    }
    return null;
  }

  private void assertTimeFormat(String time) throws
          InvalidInputException {
    try {
      LocalTime.parse(time, DateTimeFormatter.ofPattern("HH:mm:ss"));
    } catch (DateTimeParseException e) {
      throw new InvalidInputException(String.format("\"%s\" is not a valid time, only HH:mm:ss format is allowed", time), e);
    }
  }

}
