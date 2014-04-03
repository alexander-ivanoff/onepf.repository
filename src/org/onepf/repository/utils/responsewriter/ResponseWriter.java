package org.onepf.repository.utils.responsewriter;

import org.onepf.repository.utils.responsewriter.descriptors.*;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public abstract class ResponseWriter {

    private static final String API_VERSION = "1";

    public abstract void write(Writer writer, WritableHeader header, List<? extends Writable> items) throws WriteException;

    public abstract void write(ApplicationDescriptor applicationDescriptor) throws WriteException;
    public abstract void write(PurchaseDescriptor purchaseDescriptor) throws  WriteException;
    public abstract void write(DownloadDescriptor downloadDescriptor) throws  WriteException;
    public abstract void write(ReviewDescriptor reviewDescriptor) throws WriteException;

    public abstract void writeOpening(ApplicationListHeaderDescriptor descriptor) throws WriteException;
    public abstract void writeOpening(DownloadsListHeaderDescriptor descriptor) throws WriteException;
    public abstract void writeOpening(PurchasesListHeaderDescriptor descriptor) throws WriteException;
    public abstract void writeOpening(ReviewsListHeaderDescriptor descriptor) throws WriteException;

    public abstract void writeClosing(ApplicationListHeaderDescriptor descriptor) throws WriteException;
    public abstract void writeClosing(DownloadsListHeaderDescriptor descriptor) throws WriteException;
    public abstract void writeClosing(PurchasesListHeaderDescriptor descriptor) throws WriteException;
    public abstract void writeClosing(ReviewsListHeaderDescriptor descriptor) throws WriteException;

    public void writeApplications(Writer writer, List<ApplicationDescriptor> applications, String offset) throws WriteException {
        write(writer, new ApplicationListHeaderDescriptor(API_VERSION, offset), applications);
    }

    public void writePurchases(Writer writer, List<PurchaseDescriptor> purchases, String offset) throws WriteException {
        write(writer, new PurchasesListHeaderDescriptor(API_VERSION, offset), purchases);
    }

    public void writeDownloads(Writer writer, List<DownloadDescriptor> downloads, String offset) throws WriteException {
        write(writer, new DownloadsListHeaderDescriptor(API_VERSION, offset), downloads);
    }

    public void writeReviews(PrintWriter writer, List<ReviewDescriptor> reviews, String offset) throws WriteException {
        write(writer, new ReviewsListHeaderDescriptor(API_VERSION, offset), reviews);
    }



}
