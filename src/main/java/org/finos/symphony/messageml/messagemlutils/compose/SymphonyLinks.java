/*
 * Copyright 2026 MessageML - Symphony LLC
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

package org.finos.symphony.messageml.messagemlutils.compose;

import java.io.ByteArrayOutputStream;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Locale;

/**
 * Builds and repairs Symphony room and message deep links ({@code https://open.symphony.com/?...}).
 *
 * <p>The canonical form is the one Symphony itself produces when a user copies a link: stream and
 * message IDs in standard base64 with {@code =} padding, percent-encoded so that {@code /},
 * {@code +} and {@code =} become {@code %2F}, {@code %2B} and {@code %3D}. A message link carries
 * {@code streamId} and {@code messageId} and no {@code streamType}; a room link carries
 * {@code streamId} and {@code streamType} ({@code chatroom} or {@code im}).
 *
 * <p>The same class builds the links handed to the model by the messaging tools and repairs the
 * links the model writes itself, so a change to the format is a one-file fix. It is pure JDK on
 * purpose: this module must not depend on the BDK, whose {@code IdUtil} does part of the same job.
 */
public final class SymphonyLinks {

    /** The prefix every canonical deep link starts with. */
    public static final String PREFIX = "https://open.symphony.com/?";

    public static final String CHATROOM = "chatroom";
    public static final String IM = "im";

    private static final String STREAM_ID = "streamId";
    private static final String MESSAGE_ID = "messageId";
    private static final String STREAM_TYPE = "streamType";

    private static final String HOST = "open.symphony.com";
    private static final String LEGACY_SCHEME = "symphony:";

    private SymphonyLinks() {
    }

    /**
     * Builds the canonical message link.
     *
     * @param streamId  the stream ID, in standard, URL-safe or percent-encoded base64
     * @param messageId the message ID, in any of the same forms
     * @return the canonical link or {@code null} when either ID is not valid base64
     */
    public static String messageLink(String streamId, String messageId) {
        String stream = canonicalId(streamId);
        String message = canonicalId(messageId);
        if (stream == null || message == null) {
            return null;
        }
        return PREFIX + STREAM_ID + "=" + stream + "&" + MESSAGE_ID + "=" + message;
    }

    /**
     * Builds the canonical room link.
     *
     * @param streamId   the stream ID, in standard, URL-safe or percent-encoded base64
     * @param streamType {@code chatroom} or {@code im}, case-insensitive
     * @return the canonical link or {@code null} when the ID is not valid base64 or the type is
     *         neither {@code chatroom} nor {@code im}
     */
    public static String roomLink(String streamId, String streamType) {
        String stream = canonicalId(streamId);
        String type = canonicalStreamType(streamType);
        if (stream == null || type == null) {
            return null;
        }
        return PREFIX + STREAM_ID + "=" + stream + "&" + STREAM_TYPE + "=" + type;
    }

    /**
     * Repairs a link written by the model.
     *
     * <p>Only links whose target is {@code open.symphony.com} or whose scheme is {@code symphony:}
     * and that carry a {@code streamId} or {@code messageId} are touched. They are rebuilt in the
     * canonical form, with {@code streamId} first, then {@code messageId} or {@code streamType},
     * then every other parameter byte for byte in its original order. A stray {@code streamType}
     * on a message link is dropped. Anything else, including fragment URIs such as
     * {@code #phoneNumber=...}, is returned unchanged.
     *
     * <p>The query is parsed by hand: form decoding would turn a raw {@code +} into a space, which
     * is exactly the corruption being repaired.
     *
     * @param url the link destination as written
     * @return the canonical link, {@code url} unchanged or {@code null} when the link targets
     *         Symphony but cannot resolve (a {@code messageId} without a {@code streamId}, an ID that
     *         is not valid base64 or a room link without a valid {@code streamType})
     */
    public static String normalize(String url) {
        if (url == null || url.indexOf('#') >= 0) {
            return url;
        }
        String query = symphonyQuery(url);
        if (query == null) {
            return url;
        }

        String streamId = null;
        String messageId = null;
        String streamType = null;
        List<String> others = new ArrayList<>();
        for (String pair : query.split("&")) {
            if (pair.isEmpty()) {
                continue;
            }
            int eq = pair.indexOf('=');
            String name = eq < 0 ? pair : pair.substring(0, eq);
            String value = eq < 0 ? "" : pair.substring(eq + 1);
            switch (name) {
                case STREAM_ID -> streamId = streamId == null ? value : streamId;
                case MESSAGE_ID -> messageId = messageId == null ? value : messageId;
                case STREAM_TYPE -> streamType = streamType == null ? value : streamType;
                default -> others.add(pair);
            }
        }

        if (streamId == null && messageId == null) {
            return url;
        }
        if (streamId == null) {
            return null;
        }
        String link = messageId != null ? messageLink(streamId, messageId) : roomLink(streamId, streamType);
        if (link == null) {
            return null;
        }
        StringBuilder sb = new StringBuilder(link);
        others.forEach(pair -> sb.append('&').append(pair));
        return sb.toString();
    }

    /**
     * Returns the query of a Symphony deep link (possibly empty) or {@code null} when the URL does
     * not target Symphony or carries no query at all.
     */
    private static String symphonyQuery(String url) {
        String lower = url.toLowerCase(Locale.ROOT);
        String rest;
        if (lower.startsWith(LEGACY_SCHEME)) {
            rest = url.substring(LEGACY_SCHEME.length());
        } else if (lower.startsWith("https://" + HOST)) {
            rest = url.substring(("https://" + HOST).length());
        } else if (lower.startsWith("http://" + HOST)) {
            rest = url.substring(("http://" + HOST).length());
        } else {
            return null;
        }
        if (lower.startsWith("http") && !rest.isEmpty() && rest.charAt(0) != '/' && rest.charAt(0) != '?') {
            // open.symphony.com.evil.example or open.symphony.community - not our host
            return null;
        }
        int q = rest.indexOf('?');
        return q < 0 ? null : rest.substring(q + 1);
    }

    /**
     * Applies the ID canonicalization rule: percent-decode, map the URL-safe alphabet back to the
     * standard one, re-pad, validate by decoding and percent-encode.
     *
     * @return the percent-encoded standard base64 ID or {@code null} when it is not valid base64
     */
    static String canonicalId(String id) {
        if (id == null) {
            return null;
        }
        String value = id.indexOf('%') >= 0 ? percentDecode(id) : id;
        if (value == null || value.isEmpty()) {
            return null;
        }
        boolean urlSafe = (value.indexOf('-') >= 0 || value.indexOf('_') >= 0)
                && value.indexOf('+') < 0 && value.indexOf('/') < 0;
        if (urlSafe) {
            value = value.replace('-', '+').replace('_', '/');
        }
        StringBuilder padded = new StringBuilder(value);
        while (padded.length() % 4 != 0) {
            padded.append('=');
        }
        value = padded.toString();
        try {
            Base64.getDecoder().decode(value);
        } catch (IllegalArgumentException e) {
            return null;
        }
        return URLEncoder.encode(value, StandardCharsets.UTF_8);
    }

    private static String canonicalStreamType(String streamType) {
        if (streamType == null) {
            return null;
        }
        String type = streamType.toLowerCase(Locale.ROOT);
        return CHATROOM.equals(type) || IM.equals(type) ? type : null;
    }

    /**
     * Decodes {@code %XX} sequences only. {@link java.net.URLDecoder} is not used because it turns
     * {@code +} into a space.
     *
     * @return the decoded value or {@code null} on a malformed escape
     */
    private static String percentDecode(String value) {
        ByteArrayOutputStream out = new ByteArrayOutputStream(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c > 0x7F) {
                // never part of a base64 ID (writing it as one byte would hide that)
                return null;
            }
            if (c != '%') {
                out.write(c);
                continue;
            }
            if (i + 2 >= value.length()) {
                return null;
            }
            int hi = Character.digit(value.charAt(i + 1), 16);
            int lo = Character.digit(value.charAt(i + 2), 16);
            if (hi < 0 || lo < 0) {
                return null;
            }
            out.write((hi << 4) | lo);
            i += 2;
        }
        return out.toString(StandardCharsets.UTF_8);
    }
}
