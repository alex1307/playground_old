package com.ayagasha.playground.devops.files;

import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

import static junit.framework.TestCase.assertEquals;

public class TestRandomAccessFile {

    private RandomAccessFile file;

    @Before
    public void init() throws IOException {
        file = new RandomAccessFile("test.txt", "rw");
    }

    @Test
    public void testFileSize() throws Exception {

        FileChannel source = file.getChannel();
        source.position(12288);
        assertEquals(1024, source.write(get1K((byte) 'A')));
        assertEquals(1024, source.write(get1K((byte) 'B')));
        assertEquals(1024, source.write(get1K((byte) 'C')));
        assertEquals(1024, source.write(get1K((byte) 'D')));
        assertEquals(1024, source.write(get1K((byte) 'E')));
        assertEquals(1024, source.write(get1K((byte) 'F')));

        assertEquals(1024, source.write(get1K((byte) 'a')));
        assertEquals(1024, source.write(get1K((byte) 'b')));
        assertEquals(1024, source.write(get1K((byte) 'c')));
        assertEquals(1024, source.write(get1K((byte) 'd')));
        assertEquals(1024, source.write(get1K((byte) 'e')));
        assertEquals(1024, source.write(get1K((byte) 'f')));


        assertEquals(24 * 1024, source.position());
    }

    private ByteBuffer get1K(final byte value) {
        byte bytes[] = new byte[1024];
        for (int i = 0; i < bytes.length; i++) {
            bytes[i] = (i % 80 == 0) ? 13 : value;
        }

        return ByteBuffer.wrap(bytes);
    }

}
