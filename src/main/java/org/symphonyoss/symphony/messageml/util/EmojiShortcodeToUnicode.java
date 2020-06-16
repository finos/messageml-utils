package org.symphonyoss.symphony.messageml.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Class that contains HashMap which has conversion shortcode -> unicode for all supported Emojis.
 * @author cristiadu
 * @since 10/10/17
 */
public class EmojiShortcodeToUnicode {
  private static final String EMOJI_JSON_FILE = "emoji.properties";

  private static final Map<String, String> emojiShortcodeToUnicode = new HashMap<>();

  public static String getUnicode(String shortcode) {
    return emojiShortcodeToUnicode.get(shortcode);
  }

  public static Boolean hasUnicodeRepresentation(String shortcode) {
    return (emojiShortcodeToUnicode.get(shortcode) != null);
  }

  private static String emojiToUnicodeString(int... unicodes) {
    return new String(unicodes, 0, unicodes.length);
  }
  //Loads emoticons from file because the list is too big for a java class
  static {
    try (InputStream is = EmojiShortcodeToUnicode.class.getClassLoader().getResourceAsStream(EMOJI_JSON_FILE)) {
      Properties properties  = new Properties();
      properties.load(is);
      for(String key : properties.stringPropertyNames()) {
        String[] unicodeAStringArray = properties.getProperty(key).split(",");
        int[] unicodeArray = new int[unicodeAStringArray.length];
        for (int i = 0; i < unicodeAStringArray.length; i++) {
          unicodeArray[i] = Integer.decode(unicodeAStringArray[i]);
        }
        emojiShortcodeToUnicode.put(key, emojiToUnicodeString(unicodeArray));
      }
    } catch (IOException e) {}
  }
}
