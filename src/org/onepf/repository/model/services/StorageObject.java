package org.onepf.repository.model.services;

import java.io.InputStream;

/**
 * Created by ivanoff on 27.03.14.
 */
public interface StorageObject {

    InputStream asStream() throws StorageException;
    long size() throws StorageException;
}
