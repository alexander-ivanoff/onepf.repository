package org.onepf.repository.api.responsewriter;

import org.onepf.repository.api.responsewriter.descriptors.*;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.DownloadEntity;
import org.onepf.repository.api.responsewriter.entity.PurchaseEntity;
import org.onepf.repository.api.responsewriter.entity.ReviewEntity;

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
    public abstract void write(Writer writer, WritableHeader header, List<? extends Object> items) throws WriteException;

    /**
     * write applicationDescriptor to response
     *
     * @param applicationDescriptor
     * @throws WriteException
     */
    public abstract void write(ApplicationEntity applicationDescriptor) throws WriteException;

    /**
     * write purchaseDescriptor to response
     *
     * @param purchaseDescriptor
     * @throws WriteException
     */
    public abstract void write(PurchaseEntity purchaseDescriptor) throws  WriteException;

    /**
     * write downloadDescriptor to response
     *
     * @param downloadDescriptor
     * @throws WriteException
     */
    public abstract void write(DownloadEntity downloadDescriptor) throws  WriteException;

    /**
     * write reviewDescriptor to response
     *
     * @param reviewDescriptor
     * @throws WriteException
     */
    public abstract void write(ReviewEntity reviewDescriptor) throws WriteException;

    /**
     * write opening elements for ApplicationEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(ApplicationListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write opening elements for DownloadEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(DownloadsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write opening elements for PurchaseEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(PurchasesListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write opening elements for ReviewEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeOpening(ReviewsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for ApplicationEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(ApplicationListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for DownloadEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(DownloadsListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for PurchaseEntity list
     *
     * @param descriptor
     * @throws WriteException
     */
    public abstract void writeClosing(PurchasesListHeaderDescriptor descriptor) throws WriteException;

    /**
     * write closing elements for ReviewEntity list
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
    public void writeApplications(Writer writer, List<ApplicationEntity> applications, String offset) throws WriteException {
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
    public void writePurchases(Writer writer, List<PurchaseEntity> purchases, String offset) throws WriteException {
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
    public void writeDownloads(Writer writer, List<DownloadEntity> downloads, String offset) throws WriteException {
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
    public void writeReviews(PrintWriter writer, List<ReviewEntity> reviews, String offset) throws WriteException {
        write(writer, new ReviewsListHeaderDescriptor(API_VERSION, offset), reviews);
    }



}
