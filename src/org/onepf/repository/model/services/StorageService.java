package org.onepf.repository.model.services;

import org.onepf.repository.model.FileType;

import java.io.InputStream;

/**
 * Created by ivanoff on 25.03.14.
 */
public interface StorageService {

    void storeObject(String packageName, InputStream is, FileType fileType, long contentLength) throws StorageException;
    InputStream getObjectAsStream(String packageName, FileType fileType) throws StorageException;
    long getObjectSize(String packageName, FileType fileType) throws StorageException;

}
