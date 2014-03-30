package org.onepf.repository.model.services;

import java.io.InputStream;

/**
 * Created by ivanoff on 25.03.14.
 */
public interface StorageService {

    void storeObject(String objectKey, InputStream is, long contentLength) throws StorageException;
    StorageObject getObject(String objectKey) throws StorageException;

}
