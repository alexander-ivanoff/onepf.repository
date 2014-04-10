package org.onepf.repository.api.responsewriter;

import org.onepf.repository.api.responsewriter.descriptors.*;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.List;

/**
 * Abstract ResponseWriter to present server data as responses in different formats.
 *
 * @author Alexander Ivanoff
 */
public abstract class ResponseWriter {

    private static final String API_VERSION = "1";

    /**
     * Generic method to write List of writable objects to response.
     *
     * @param writer - writer to write objects into
     * @param header - writeable object used to enclose entire list
     * @param items - writable objects to write as xml
     * @throws WriteException
     */
    public abstract void write(Writer writer, WritableHeader header, List<? extends Writable> items) throws WriteException;

    /**
     * write applicationDescriptor to response
     *
     * @param applicationDescriptor
     * @throws WriteException
     */
    public abstract void write(ApplicationDescriptor applicationDescriptor) throws WriteException;

    /**
     * write purchaseDescriptor to response
     *
     * @param purchaseDescriptor
     * @throws WriteException
     */
    public abstract void write(PurchaseDescriptor purchaseDescriptor) throws  WriteException;

    /**
     * write downloadDescriptor to response
     *
     * @param downloadDescriptor
     * @throws WriteException
     */
    public abstract void write(DownloadDescriptor downloadDescriptor) throws  WriteException;

    /**
     * write reviewDescriptor to response
     *
     * @param reviewDescriptor
     * @throws WriteException
     */
    public abstract void write(ReviewDescriptor reviewDescriptor) throws WriteException;

    /**
     * write opening elements for Application list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(ApplicationListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write opening elements for Download list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(DownloadsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write opening elements for Purchase list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(PurchasesListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write opening elements for Review list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(ReviewsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for Application list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(ApplicationListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for Download list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(DownloadsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for Purchase list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(PurchasesListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for Review list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(ReviewsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write applications list to the writer
     *
     * @param writer - writer to write response into
     * @param applications - list of applications to write into response
     * @param offset - offset to the next page
     * @throws WriteException
     */
    public void writeApplications(Writer writer, List<ApplicationDescriptor> applications, String offset) throws WriteException {
        write(writer, new ApplicationListHeaderDescriptor(API_VERSION, offset), applications);
    }

    /**
     * write purchases list to the writer
     *
     * @param writer - writer to write response into
     * @param purchases - list of purchases to write into response
     * @param offset - offset to the next page
     * @throws WriteException
     */
    public void writePurchases(Writer writer, List<PurchaseDescriptor> purchases, String offset) throws WriteException {
        write(writer, new PurchasesListHeaderDescriptor(API_VERSION, offset), purchases);
    }

    /**
     * write downloads list to the writer
     *
     * @param writer - writer to write response into
     * @param downloads - list of purchases to write into response
     * @param offset - offset to the next page
     * @throws WriteException
     */
    public void writeDownloads(Writer writer, List<DownloadDescriptor> downloads, String offset) throws WriteException {
        write(writer, new DownloadsListHeaderDescriptor(API_VERSION, offset), downloads);
    }

    /**
     * write reviews list to the writer
     *
     * @param writer - writer to write response into
     * @param reviews - list of purchases to write into response
     * @param offset - offset to the next page
     * @throws WriteException
     */
    public void writeReviews(PrintWriter writer, List<ReviewDescriptor> reviews, String offset) throws WriteException {
        write(writer, new ReviewsListHeaderDescriptor(API_VERSION, offset), reviews);
    }



}
