package org.onepf.repository.utils.responsewriter;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.Writer;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class ResponseWriter {

    public abstract void write(Writer writer, String name, Map<String, String> header, List<? extends Writable> items) throws WriteException;

    public abstract void write(ApplicationDescriptor applicationDescriptor) throws WriteException;
    public abstract void write(PurchaseDescriptor purchaseDescriptor) throws  WriteException;
    public abstract void write(DownloadDescriptor downloadDescriptor) throws  WriteException;

    public void writeApplications(Writer writer, List<ApplicationDescriptor> applications, long lastUpdated) throws WriteException {
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("version", "1");
        headers.put("platform", "android");
        headers.put("last-updated", String.valueOf(lastUpdated));
        write(writer, "application-list", headers, applications);
    }

    public void writePurchases(Writer writer, List<PurchaseDescriptor> purhases) throws WriteException {
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("version", "1");
        write(writer, "purchases", headers, purhases);
    }

    public void writeDownloads(Writer writer, List<DownloadDescriptor> downloads) throws WriteException {
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("version", "1");
        write(writer, "downloads", headers, downloads);
    }



}
