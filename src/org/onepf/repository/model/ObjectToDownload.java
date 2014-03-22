package org.onepf.repository.model;

import java.io.FileNotFoundException;
import java.io.InputStream;

/**
 * Created by ivanoff on 13.03.14.
 */
public abstract class ObjectToDownload {


        public static class ObjectOptions {
            public String packageName;
            public FileType fileType = FileType.APPDF;

    }

        abstract public void init(ObjectOptions objectOptions) throws FileNotFoundException;
        abstract public InputStream getAsStream() throws FileNotFoundException;
        abstract public String getName() throws FileNotFoundException;
        abstract public int getSize() throws  FileNotFoundException;

}
