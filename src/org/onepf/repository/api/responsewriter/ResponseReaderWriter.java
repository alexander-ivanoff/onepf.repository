package org.onepf.repository.api.responsewriter;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Created by ivanoff on 17.04.14.
 */
public interface ResponseReaderWriter<T> {

    void write(OutputStream os, T objectToWrite) throws WriteException;

    public <T> T read( Class<T> docClass, InputStream inputStream ) throws WriteException;
}
