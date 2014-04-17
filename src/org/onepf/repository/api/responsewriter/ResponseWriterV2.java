package org.onepf.repository.api.responsewriter;

import org.onepf.repository.api.responsewriter.WriteException;

import java.io.OutputStream;

/**
 * Created by ivanoff on 17.04.14.
 */
public interface ResponseWriterV2<T> {

    void write(OutputStream os, T objectToWrite) throws WriteException;
}
