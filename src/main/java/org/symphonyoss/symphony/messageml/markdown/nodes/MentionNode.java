/*
 * Copyright 2016-2017 MessageML - Symphony LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.symphonyoss.symphony.messageml.markdown.nodes;

import org.apache.commons.lang3.StringUtils;
import org.commonmark.node.CustomNode;

/**
 * Class representing a Markdown node for user mentions.
 *
 * @author lukasz
 * @since 3/30/17
 */
public class MentionNode extends CustomNode {

  private static final String PREFIX = "@";
  private final String prettyName;

  private final String screenName;
  private final String email;
  private final long uid;

  public MentionNode(long uid) {
    this(uid, null, null, null);
  }

  public MentionNode(long uid, String prettyName, String screenName, String email) {
    this.uid = uid;
    this.prettyName = prettyName;
    this.screenName = screenName;
    this.email = email;
  }

  public String getPrettyName() {
    return prettyName;
  }

  public String getScreenName() {
    return screenName;
  }

  public String getEmail() {
    return email;
  }

  public long getUid() {
    return uid;
  }

  public String getText() {
    return StringUtils.isNotBlank(prettyName) ? PREFIX + prettyName : "";
  }

}
