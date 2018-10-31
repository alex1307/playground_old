package com.ayagasha.playground.devops.files;

import com.ayagasha.playground.devops.lambdas.ByteUnit;
import com.ayagasha.playground.devops.lambdas.FileSplitExecutor;
import org.junit.Test;

import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static junit.framework.TestCase.assertTrue;


public class TestFileSplitter {


    FileSplitExecutor executor;


    @Test
    public void testText() throws Exception {
        executor = new FileSplitExecutor("files/test.txt", 1, ByteUnit.KILOBYTE);
        Future<Boolean> future = executor.execute();
        assertTrue(future.get(6, TimeUnit.SECONDS));
    }

    @Test
    public void testText4() throws Exception {
        executor = new FileSplitExecutor("files/test.txt", 4, ByteUnit.KILOBYTE);
        Future<Boolean> future = executor.execute();
        assertTrue(future.get(6, TimeUnit.SECONDS));
    }

    @Test
    public void testText2() throws Exception {
        executor = new FileSplitExecutor("files/test_2.txt", 1, ByteUnit.KILOBYTE);
        Future<Boolean> future = executor.execute();
        assertTrue(future.get(6, TimeUnit.SECONDS));
    }

    @Test
    public void testText3() throws Exception {
        executor = new FileSplitExecutor("files/test_2.txt", 4, ByteUnit.KILOBYTE);
        Future<Boolean> future = executor.execute();
        assertTrue(future.get(6, TimeUnit.SECONDS));
    }


    @Test
    public void testImage() throws Exception {
        executor = new FileSplitExecutor("files/img.tiff", 1, ByteUnit.MEGABYTE);
        Future<Boolean> future = executor.execute();
        assertTrue(future.get(6, TimeUnit.SECONDS));
    }


}
