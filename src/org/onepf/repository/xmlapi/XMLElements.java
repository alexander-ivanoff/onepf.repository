package org.onepf.repository.xmlapi;

/**
 * Created by ivanoff on 02.04.14.
 */
public interface XMLElements {

    public static interface Application {
        public static final String ELEMENT_NAME = "application";

        public static final String FIELD_PACKAGE = "package";
        public static final String FIELD_LAST_UPDATED = "last-updated";
    }

    public static interface BaseHeader {
        public static final String FIELD_VERSION = "version";
        public static final String FIELD_OFFSET = "offset";
    }

    public static interface ApplicationListHeader {
        public static final String ELEMENT_NAME = "application-list";
    }

    public static interface DownloadsListHeader {
        public static final String ELEMENT_NAME = "downloads";
    }

    public static interface PurchasesListHeader {
        public static final String ELEMENT_NAME = "purchases";
    }

    public static interface ReviewsListHeader {
        public static final String ELEMENT_NAME = "reviews";
    }
}