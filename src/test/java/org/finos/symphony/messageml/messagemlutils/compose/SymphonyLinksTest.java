package org.finos.symphony.messageml.messagemlutils.compose;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class SymphonyLinksTest {

    /** Copied from SDA ("Copy link" on a message). */
    private static final String GOLDEN_MESSAGE_LINK = "https://open.symphony.com/?streamId=cGLj3wbzGI1lO7AcQM4Brn%2F%2F%2FmU6c3uEdA%3D%3D&messageId=8bMFOfcc4WD%2BpJsS32IzWn%2F%2F%2Fl7koV4kbQ%3D%3D";

    /** Copied from SDA ("Copy link" on a room). */
    private static final String GOLDEN_ROOM_LINK = "https://open.symphony.com/?streamId=R2L2gw1AdxrSEBlcaX3lpn%2F%2F%2Fn22%2F%2BhXdA%3D%3D&streamType=chatroom";

    private static final String STREAM_STANDARD = "cGLj3wbzGI1lO7AcQM4Brn///mU6c3uEdA==";
    private static final String STREAM_URL_SAFE = "cGLj3wbzGI1lO7AcQM4Brn___mU6c3uEdA";
    private static final String STREAM_ENCODED = "cGLj3wbzGI1lO7AcQM4Brn%2F%2F%2FmU6c3uEdA%3D%3D";
    private static final String MESSAGE_STANDARD = "8bMFOfcc4WD+pJsS32IzWn///l7koV4kbQ==";
    private static final String MESSAGE_URL_SAFE = "8bMFOfcc4WD-pJsS32IzWn___l7koV4kbQ";
    private static final String MESSAGE_ENCODED = "8bMFOfcc4WD%2BpJsS32IzWn%2F%2F%2Fl7koV4kbQ%3D%3D";

    private static final String ROOM_STANDARD = "R2L2gw1AdxrSEBlcaX3lpn///n22/+hXdA==";

    @Test
    void messageLink_fromUrlSafeIds_matchesGolden() {
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.messageLink(STREAM_URL_SAFE, MESSAGE_URL_SAFE));
    }

    @Test
    void messageLink_fromStandardIds_matchesGolden() {
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.messageLink(STREAM_STANDARD, MESSAGE_STANDARD));
    }

    @Test
    void messageLink_fromEncodedIds_isNotDoubleEncoded() {
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.messageLink(STREAM_ENCODED, MESSAGE_ENCODED));
    }

    @Test
    void messageLink_invalidId_returnsNull() {
        assertNull(SymphonyLinks.messageLink(STREAM_STANDARD, "not base64!"));
        assertNull(SymphonyLinks.messageLink(null, MESSAGE_STANDARD));
    }

    @Test
    void roomLink_fromStandardId_matchesGolden() {
        assertEquals(GOLDEN_ROOM_LINK, SymphonyLinks.roomLink(ROOM_STANDARD, "chatroom"));
    }

    @Test
    void roomLink_fromUrlSafeIdAndUpperCaseType_matchesGolden() {
        assertEquals(GOLDEN_ROOM_LINK, SymphonyLinks.roomLink("R2L2gw1AdxrSEBlcaX3lpn___n22_-hXdA", "CHATROOM"));
    }

    @Test
    void roomLink_im() {
        assertEquals("https://open.symphony.com/?streamId=" + STREAM_ENCODED + "&streamType=im",
                SymphonyLinks.roomLink(STREAM_URL_SAFE, "IM"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"mim", "ROOM", ""})
    void roomLink_unknownType_returnsNull(String type) {
        assertNull(SymphonyLinks.roomLink(STREAM_STANDARD, type));
    }

    @Test
    void roomLink_nullType_returnsNull() {
        assertNull(SymphonyLinks.roomLink(STREAM_STANDARD, null));
    }

    @Test
    void canonicalId_rejectsMixedAlphabetsMalformedEscapesAndNonAscii() {
        assertNull(SymphonyLinks.canonicalId("ab-d/fgh"));
        assertNull(SymphonyLinks.canonicalId("abcd%2"));
        assertNull(SymphonyLinks.canonicalId("abcd%ZZ"));
        assertNull(SymphonyLinks.canonicalId("abcdéfgh"));
        assertNull(SymphonyLinks.canonicalId(""));
        assertNull(SymphonyLinks.canonicalId("a"));
    }

    @Test
    void normalize_rawBase64_isReEncoded() {
        String url = "https://open.symphony.com/?streamId=" + STREAM_STANDARD + "&messageId=" + MESSAGE_STANDARD;
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.normalize(url));
    }

    @Test
    void normalize_urlSafeIds_areConverted() {
        String url = "https://open.symphony.com/?streamId=" + STREAM_URL_SAFE + "&messageId=" + MESSAGE_URL_SAFE;
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.normalize(url));
    }

    @Test
    void normalize_canonicalLinks_areIdempotent() {
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.normalize(GOLDEN_MESSAGE_LINK));
        assertEquals(GOLDEN_ROOM_LINK, SymphonyLinks.normalize(GOLDEN_ROOM_LINK));
    }

    @Test
    void normalize_legacySchemeIsUpgraded() {
        assertEquals(GOLDEN_ROOM_LINK, SymphonyLinks.normalize("symphony://?streamId=" + ROOM_STANDARD + "&streamType=chatroom"));
    }

    @Test
    void normalize_hostVariantsAreCanonicalized() {
        String expected = SymphonyLinks.roomLink(STREAM_STANDARD, "im");
        assertEquals(expected, SymphonyLinks.normalize("http://open.symphony.com?streamType=IM&streamId=" + STREAM_URL_SAFE));
        assertEquals(expected, SymphonyLinks.normalize("HTTPS://Open.Symphony.com/?streamId=" + STREAM_ENCODED + "&streamType=im"));
    }

    @Test
    void normalize_strayStreamTypeOnMessageLink_isDropped() {
        String url = "https://open.symphony.com/?streamType=chatroom&messageId=" + MESSAGE_URL_SAFE + "&streamId=" + STREAM_URL_SAFE;
        assertEquals(GOLDEN_MESSAGE_LINK, SymphonyLinks.normalize(url));
    }

    @Test
    void normalize_otherParameters_areKeptByteForByte() {
        String url = "https://open.symphony.com/?foo=a+b%2F&streamId=" + STREAM_URL_SAFE + "&messageId=" + MESSAGE_URL_SAFE + "&flag";
        assertEquals(GOLDEN_MESSAGE_LINK + "&foo=a+b%2F&flag", SymphonyLinks.normalize(url));
    }

    @Test
    void normalize_messageIdWithoutStreamId_isUnresolvable() {
        assertNull(SymphonyLinks.normalize("https://open.symphony.com/?messageId=" + MESSAGE_URL_SAFE));
    }

    @Test
    void normalize_invalidBase64_isUnresolvable() {
        assertNull(SymphonyLinks.normalize("https://open.symphony.com/?streamId=not*base64&streamType=chatroom"));
        assertNull(SymphonyLinks.normalize("https://open.symphony.com/?streamId=" + STREAM_URL_SAFE + "&messageId=%%%"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"&streamType=room", "&streamType=", ""})
    void normalize_roomLinkWithoutValidStreamType_isUnresolvable(String typeParam) {
        assertNull(SymphonyLinks.normalize("https://open.symphony.com/?streamId=" + STREAM_URL_SAFE + typeParam));
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "https://open.symphony.com/#phoneNumber=+33612345678",
            "https://open.symphony.com/?streamId=abc#fragment",
            "https://open.symphony.com/?userId=12345",
            "https://open.symphony.com/?startChat=12345,67890",
            "https://open.symphony.com/app/foo",
            "https://open.symphony.com",
            "symphony://?userId=12345",
            "https://open.symphony.com.evil.example/?streamId=" + STREAM_STANDARD + "&streamType=chatroom",
            "https://example.com/?streamId=" + STREAM_STANDARD + "&streamType=chatroom",
            "https://symphony.com/",
            "mailto:jane@example.com"
    })
    void normalize_nonDeepLinks_areUntouched(String url) {
        assertEquals(url, SymphonyLinks.normalize(url));
    }

    @Test
    void normalize_null_returnsNull() {
        assertNull(SymphonyLinks.normalize(null));
    }
}
