package org.onepf.repository.model;

import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.api.responsewriter.descriptors.ReviewDescriptor;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by ivanoff on 12.03.14.
 */
public class GetReviewsRequestHandler extends BaseRequestHandler {


    public GetReviewsRequestHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
    }

    public List<ReviewDescriptor> getReviews(String packageName, int pageHash) throws DataException {
        long time = System.currentTimeMillis();
        ArrayList<ReviewDescriptor> reviews = dataService.getReviews(packageName, pageHash);
        System.out.println("List purchases time: " + (System.currentTimeMillis() - time));  // TODO move to Log4J
        return reviews;
    }
}
