package org.onepf.repository.utils.responsewriter;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.ReviewDescriptor;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class ResponseWriter {

    public abstract void write(Writer writer, String name, Map<String, String> header, List<? extends Writable> items) throws WriteException;

    public abstract void write(ApplicationDescriptor applicationDescriptor) throws WriteException;
    public abstract void write(PurchaseDescriptor purchaseDescriptor) throws  WriteException;
    public abstract void write(DownloadDescriptor downloadDescriptor) throws  WriteException;
    public abstract void write(ReviewDescriptor reviewDescriptor) throws WriteException;

    public void writeApplications(Writer writer, List<ApplicationDescriptor> applications, String lastUpdated, String offset) throws WriteException {
        Map<String, String> headers = new HashMap<String, String>();
        writeWithBasicHeaders(writer, "application-list", applications, headers, lastUpdated, offset);
    }

    public void writePurchases(Writer writer, List<PurchaseDescriptor> purhases, String lastUpdated, String offset) throws WriteException {
        writeWithBasicHeaders(writer, "purchases", purhases, lastUpdated, offset);
    }

    public void writeDownloads(Writer writer, List<DownloadDescriptor> downloads, String lastUpdated, String offset) throws WriteException {
        writeWithBasicHeaders(writer, "downloads", downloads, lastUpdated, offset);
    }

    public void writeReviews(PrintWriter writer, List<ReviewDescriptor> reviews, String lastUpdated, String offset) throws WriteException {
        writeWithBasicHeaders(writer, "reviews", reviews, lastUpdated, offset);
    }

    public void writeWithBasicHeaders(Writer writer, String name, List<? extends Writable> items, String lastUpdated, String offset) throws WriteException {
        writeWithBasicHeaders(writer, name, items, null, lastUpdated, offset);
    }

    public void writeWithBasicHeaders(Writer writer, String name, List<? extends Writable> items, Map<String, String> headers, String lastUpdated, String offset) throws WriteException {
        if (headers == null) {
            headers = new HashMap<String, String>(3);
        }
        headers.put("version", "1");
        if (offset != null) {
            headers.put("last-updated", lastUpdated);
        }
        if (offset != null) {
            headers.put("offset", offset);
        }
        write(writer, name, headers, items);
    }
}
