package org.onepf.repository.utils.responsewriter.descriptors;

import org.onepf.repository.utils.responsewriter.ResponseWriter;
import org.onepf.repository.utils.responsewriter.Writable;
import org.onepf.repository.utils.responsewriter.WriteException;

/**
 * Created by ivanoff on 12.03.14.
 */
public class PurchaseDescriptor implements Writable {
    public String packageName;
    public long dateTime;
    public String id;
    public String version;
    public int build;
    public long lastUpdate;
    public String deviceModel;
    public String deviceName;
    public String country;
    public String userPrice;
    public String userCurrency;
    public String innerPrice;
    public String innerCurrency;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }
}
