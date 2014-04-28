package org.onepf.repository.appstorelooter;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.onepf.repository.api.responsewriter.entity.*;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.model.RepositoryFactory;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.mysql.SqlDataService;

import java.io.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * This class perform statistics (downloads, reviews and purchases) requests from appstores.
 *
 * @author Ruslan Sayfutdinov
 */
public class GetStatisticsRequest implements Runnable {
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private final Logger alarmCauseLogger = LogManager.getLogger("AlarmCauseLogger");
    private final Logger logger = LogManager.getLogger(GetAppListRequest.class.getName());

    private final AppstoreEntity appstore;
    private final HttpClient httpClient;
    private final HttpContext httpContext;
    private final RepositoryFactory repositoryFactory;
    private final File tempDir;

    private final Random random = new Random();

    public GetStatisticsRequest(RepositoryFactory repositoryFactory,
                                HttpClient httpClient,
                                AppstoreEntity appstore,
                                File tempDir) {
        this.repositoryFactory = repositoryFactory;
        this.httpClient = httpClient;
        this.httpContext = new BasicHttpContext();
        this.appstore = appstore;
        this.tempDir = tempDir;
    }

    @Override
    public void run() {
        processFeed(FeedType.DOWNLOADS);
        processFeed(FeedType.PURCHASES);
        processFeed(FeedType.REVIEWS);
    }

    private void processFeed(FeedType feedType) {
        XmlResponseReaderWriter xmlResponseReaderWriter = feedType.getXmlReaderWriter();
        try {
            LastStatisticsUpdateEntity lastStatisticsUpdate =
                    repositoryFactory.getDataService().getLastStatisticsUpdate(appstore.getAppstoreId(), feedType);
            if (!tempDir.exists()) {
                tempDir.mkdirs();
            }
            List<String> filesList = new ArrayList<String>();
            String requestUrl = feedType.getApiMapping().getMethodUrl(appstore.getOpenaepUrl());
            boolean wasError = false;
            while (true) {
                // Perform GET request to API
                HttpGet httpGet = new HttpGet(requestUrl);
                httpGet.addHeader("authToken", appstore.getAppstoreAccessToken());
                HttpResponse response = httpClient.execute(httpGet, httpContext);

                int result = response.getStatusLine().getStatusCode();

                if (result == HttpStatus.SC_OK) {
                    String filename = String.format("%s-%s-%d.xml", appstore.getAppstoreId(), feedType, random.nextInt());
                    File tempFile = storeToTempDir(response.getEntity().getContent(), filename);
                    filesList.add(filename);
                    BaseListEntity statisticsList = (BaseListEntity)xmlResponseReaderWriter.read(feedType.getListEntity(),
                            new FileInputStream(tempFile));
                    if (statisticsList.getOffset() == null ||
                            (lastStatisticsUpdate != null && statisticsList.getOffset().equals(lastStatisticsUpdate.getPrevOffset()))) {
                        break;
                    } else {
                        requestUrl = statisticsList.getOffset();
                    }
                } else {
                    alarmCauseLogger.error("Failed to load {} from {}, reason {}", feedType.toString(), appstore.getAppstoreId(),
                            response.getStatusLine().toString());
                    wasError = true;
                    break;
                }
            }
            if (!wasError) {
                Collections.reverse(filesList);
                for (String filename : filesList) {
                    BaseListEntity statisticsList = (BaseListEntity)xmlResponseReaderWriter.read(feedType.getListEntity(),
                            new FileInputStream(new File(tempDir, filename)));

                    if (lastStatisticsUpdate == null) {
                        lastStatisticsUpdate = new LastStatisticsUpdateEntity(
                                appstore.getAppstoreId(),
                                feedType,
                                0,
                                null,
                                statisticsList.getOffset()
                        );
                    }
                    switch (feedType) {
                        case DOWNLOADS:
                            processDownloads(statisticsList, lastStatisticsUpdate);
                            break;
                        case REVIEWS:
                            processReviews(statisticsList, lastStatisticsUpdate);
                            break;
                        case PURCHASES:
                            processPurchases(statisticsList, lastStatisticsUpdate);
                            break;
                    }

                    lastStatisticsUpdate = null;
                }
            }
        } catch (Exception e) {
            alarmCauseLogger.error("Failed to get statistics from {}, reason {}", appstore.getAppstoreId(), e.getMessage());
        }
    }

    private void processDownloads(BaseListEntity statisticsList, LastStatisticsUpdateEntity lastStatisticsUpdate) throws DataException {
        DownloadListEntity downloadList = (DownloadListEntity)statisticsList;
        List<DownloadEntity> downloads = downloadList.getDownload();
        int start = downloads.size() - lastStatisticsUpdate.getLastResponseCount() - 1;
        for (int i = start; i >= 0; --i) {
            //TODO move all work with database to dataService
            Session session = ((SqlDataService) repositoryFactory.getDataService()).getSession();
            try {
                DownloadEntity downloadEntity = downloads.get(i);
                ApplicationEntity app =
                        repositoryFactory.getDataService().getApplicationsLog(downloadEntity.getPackageName(), -1).get(0);
                // add homeStoreId to download
                downloadEntity.setHomeStoreId(app.getAppstoreId());
                session.beginTransaction();
                session.save(downloadEntity);
                lastStatisticsUpdate.setLastResponseCount(lastStatisticsUpdate.getLastResponseCount() + 1);
                lastStatisticsUpdate.setLastResponseDatetime(DATE_FORMAT.format(new Date(System.currentTimeMillis())));
                session.saveOrUpdate(lastStatisticsUpdate); //
                session.getTransaction().commit();
                session.close();
            } catch (RuntimeException e) {
                session.getTransaction().rollback();
                throw e;
            }
        }
    }

    private void processReviews(BaseListEntity statisticsList, LastStatisticsUpdateEntity lastStatisticsUpdate) throws DataException {
        ReviewsListEntity reviewList = (ReviewsListEntity)statisticsList;
        List<ReviewEntity> reviews = reviewList.getReview();
        int start = reviews.size() - lastStatisticsUpdate.getLastResponseCount() - 1;
        for (int i = start; i >= 0; --i) {
            //TODO move all work with database to dataService
            Session session = ((SqlDataService) repositoryFactory.getDataService()).getSession();
            try {
                ReviewEntity reviewEntity = reviews.get(i);
                ApplicationEntity app =
                        repositoryFactory.getDataService().getApplicationsLog(reviewEntity.getPackageName(), -1).get(0);
                // add homeStoreId to review
                reviewEntity.setHomeStoreId(app.getAppstoreId());
                session.beginTransaction();
                session.save(reviewEntity);
                lastStatisticsUpdate.setLastResponseCount(lastStatisticsUpdate.getLastResponseCount() + 1);
                lastStatisticsUpdate.setLastResponseDatetime(DATE_FORMAT.format(new Date(System.currentTimeMillis())));
                session.saveOrUpdate(lastStatisticsUpdate); //
                session.getTransaction().commit();
                session.close();
            } catch (RuntimeException e) {
                session.getTransaction().rollback();
                throw e;
            }
        }
    }

    private void processPurchases(BaseListEntity statisticsList, LastStatisticsUpdateEntity lastStatisticsUpdate) throws DataException {
        PurchaseListEntity purchaseList = (PurchaseListEntity)statisticsList;
        List<PurchaseEntity> purchases = purchaseList.getPurchase();
        int start = purchases.size() - lastStatisticsUpdate.getLastResponseCount() - 1;
        for (int i = start; i >= 0; --i) {
            //TODO move all work with database to dataService
            Session session = ((SqlDataService) repositoryFactory.getDataService()).getSession();
            try {
                PurchaseEntity purchaseEntity = purchases.get(i);
                ApplicationEntity app =
                        repositoryFactory.getDataService().getApplicationsLog(purchaseEntity.getPackageName(), -1).get(0);
                // add homeStoreId to purchase
                purchaseEntity.setHomeStoreId(app.getAppstoreId());
                session.beginTransaction();
                session.save(purchaseEntity);
                lastStatisticsUpdate.setLastResponseCount(lastStatisticsUpdate.getLastResponseCount() + 1);
                lastStatisticsUpdate.setLastResponseDatetime(DATE_FORMAT.format(new Date(System.currentTimeMillis())));
                session.saveOrUpdate(lastStatisticsUpdate); //
                session.getTransaction().commit();
                session.close();
            } catch (RuntimeException e) {
                session.getTransaction().rollback();
                throw e;
            }
        }
    }

    /**
     * Store XML file from input stream to temporary directory
     *
     * @param is
     * @param filename
     * @return stored file object
     * @throws java.io.IOException
     */
    private File storeToTempDir(InputStream is, String filename) throws IOException {
        File uploadedFile = new File(tempDir,  filename);
        ReadableByteChannel rbc = Channels.newChannel(is);
        FileOutputStream fos = new FileOutputStream(uploadedFile);
        fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
        return uploadedFile;
    }
}
