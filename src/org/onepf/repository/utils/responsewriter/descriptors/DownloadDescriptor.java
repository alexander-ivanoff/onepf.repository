package org.onepf.repository.utils.responsewriter.descriptors;

import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.Writable;
import org.onepf.repository.utils.responsewriter.WriteException;

/**
 * Created by ivanoff on 12.03.14.
 */
public class DownloadDescriptor implements Writable {
    public String packageName;
    public String dateTime;
    public String isUpdate;
    public String version;
    public int build;
    public String lastUpdate;
    public String deviceModel;
    public String deviceName;
    public String country;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }
}
