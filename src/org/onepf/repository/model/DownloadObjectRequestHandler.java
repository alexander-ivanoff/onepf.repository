package org.onepf.repository.model;

import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageObject;
import org.onepf.repository.model.services.StorageService;

import java.io.FileNotFoundException;
import java.io.InputStream;

/**
 * Created by ivanoff on 12.03.14.
 */
public class DownloadObjectRequestHandler extends BaseRequestHandler {

    public static class ObjectOptions {
        public String packageName;
        public FileType fileType = FileType.APPDF;

    }

    private ObjectOptions options;
    StorageObject storageObject;

    public DownloadObjectRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public void init(ObjectOptions options) throws StorageException{
        this.options = options;
        storageObject = storageService.getObject(options.packageName, options.fileType);
    }

    public InputStream getAsStream() throws StorageException {
        return storageObject.asStream();
    }


    public String getName() throws FileNotFoundException {
        return options.packageName + '.' + options.fileType.extention();
    }

    public long getSize() throws FileNotFoundException, StorageException {
        return storageObject.size();
    }

}
