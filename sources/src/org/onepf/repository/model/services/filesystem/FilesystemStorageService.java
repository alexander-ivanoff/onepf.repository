package org.onepf.repository.model.services.filesystem;

import org.apache.commons.io.IOUtils;
import org.onepf.repository.model.services.StorageException;
import org.onepf.repository.model.services.StorageObject;
import org.onepf.repository.model.services.StorageService;

import java.io.*;

/**
 *
 * Implementation of StorageService and StorageObject with local file system.
 *
 * @author Alexander Ivanoff
 */
public class FilesystemStorageService implements StorageService {



    public static class FileStorageObject extends File implements StorageObject {

        protected FileStorageObject(File file, String path) {
            super(file, path);
        }

        @Override
        public InputStream asStream() throws StorageException {
            try {
                return new FileInputStream(this);
            } catch (FileNotFoundException e) {
                throw new StorageException(e);
            }
        }

        @Override
        public long size() throws StorageException {
            return length();
        }
    }


    private File targetDir;

    public FilesystemStorageService(FilesystemOptions options) {
        targetDir = new File(options.targetPath);
        if (!targetDir.exists()) {
            targetDir.mkdirs();
        }

    }

    @Override
    public void storeObject(String objectKey, InputStream is, long contentLength) throws StorageException {
        OutputStream fos = null;
            try {
                //File packageDir = new File(targetDir, packageName);
                //packageDir.mkdirs();

                File targetFile = new File(targetDir, objectKey);

                targetFile.getParentFile().mkdirs();
                targetFile.createNewFile();

                fos = new FileOutputStream(targetFile);

                IOUtils.copy(is, fos);
            } catch (IOException e) {
                throw new StorageException(e);
            } finally {
                try { if (fos != null) fos.close(); } catch(Exception e) { }
                try { if (is != null) is.close(); } catch(Exception e) { }
            }
    }

    @Override
    public StorageObject getObject(String objectKey) throws StorageException {
        return new FileStorageObject(targetDir, objectKey);
    }

}
