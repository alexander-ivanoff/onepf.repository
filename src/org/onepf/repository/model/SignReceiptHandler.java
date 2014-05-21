package org.onepf.repository.model;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.params.ClientPNames;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.PoolingClientConnectionManager;
import org.apache.http.params.HttpParams;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.onepf.repository.ApiMapping;
import org.onepf.repository.api.responsewriter.WriteException;
import org.onepf.repository.api.responsewriter.entity.AppstoreEntity;
import org.onepf.repository.api.responsewriter.entity.ObjectFactory;
import org.onepf.repository.api.responsewriter.entity.ReceiptEntity;
import org.onepf.repository.api.xmlapi.XmlResponseReaderWriter;
import org.onepf.repository.appstorelooter.RequesterUtils;
import org.onepf.repository.model.services.DataException;
import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.Error;
import org.onepf.repository.model.services.StorageService;
import org.onepf.repository.utils.Utils;
import sun.misc.BASE64Decoder;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
import java.util.Date;

/**
 *  Provide get applications request to underlying DataService
* @author Alexander Ivanoff on 12.03.14.
 */
public class SignReceiptHandler extends BaseRequestHandler {

    private boolean initialized = false;

    private XmlResponseReaderWriter<ReceiptEntity> xmlRequestReader;

    private HttpClient httpClient;

    ObjectMapper mapper = new ObjectMapper();

    private static final String SIGNATURE_ALGORITHM = "SHA1withRSA";
    private static final String KEY_ALGORITHM = "RSA";

    private final Logger logger = LogManager.getLogger(SignReceiptHandler.class.getName());

    private BASE64Decoder decoder = new BASE64Decoder();

    public SignReceiptHandler(DataService dataService, StorageService storageService) {
        super(dataService, storageService);
        PoolingClientConnectionManager cm = new PoolingClientConnectionManager();
        cm.setMaxTotal(100);
        cm.setDefaultMaxPerRoute(20);
        httpClient = new DefaultHttpClient(cm);
        // ser redirectiong to true
        HttpParams httpClientParams = httpClient.getParams();
        //HttpClientParams.setRedirecting(httpClientParams, true);
        httpClientParams.setParameter(ClientPNames.HANDLE_REDIRECTS, true);
        httpClientParams.setParameter(ClientPNames.ALLOW_CIRCULAR_REDIRECTS, true);

        initialized = true;
        try {
            xmlRequestReader = new XmlResponseReaderWriter<ReceiptEntity>(ObjectFactory._Receipt_QNAME, ReceiptEntity.class.getPackage().getName());
        } catch (WriteException e) {
            e.printStackTrace();
            initialized = false;
        }
    }


    public ReceiptEntity getSignedReceipt(AppstoreEntity appstore, String receiptBody, OutputStream os) throws DataException {
        ReceiptEntity receiptToSign = null;
        ReceiptEntity signedReceipt = null;
        try {
            receiptToSign = (ReceiptEntity)xmlRequestReader.read(ReceiptEntity.class, new ByteArrayInputStream(receiptBody.toString().getBytes()));
            if (!appstore.getAppstoreId().equals(receiptToSign.getDistributorAppstore())) {
                throw new DataException(Error.BAD_REQUEST.withMessage("Incorrect appstore in receipt"));
            }
            boolean verified = checkSignature(appstore.getPublickKey(), receiptToSign.getReceiptData(), receiptToSign.getDistributorSignature());
            if (verified) {
                ReceiptData receiptData = mapper.readValue(receiptToSign.getReceiptData(), ReceiptData.class);
                AppstoreEntity homeStore = dataService.getHomeStore(receiptData.packageName);
                if (appstore == null) {
                    throw new DataException(Error.INTERNAL_ERROR);
                }
                signedReceipt = signByHomeStore(homeStore, receiptBody);
                if (signedReceipt != null) {
                    receiptToSign.setDeveloperAppstore(signedReceipt.getDeveloperAppstore());
                    receiptToSign.setDeveloperSignature(signedReceipt.getDeveloperSignature());
                }
            } else {
                throw new DataException(Error.BAD_REQUEST.withMessage("Bad signature"));
            }
        } catch (WriteException e) {
            throw new DataException(Error.BAD_REQUEST.withMessage("Bad request body"));
        } catch (IOException e) {
            throw new DataException(Error.BAD_REQUEST.withMessage("Bad receipt data"));
        } finally {
            if (receiptToSign != null) {
                receiptToSign.setDateTime(Utils.sqlFormattedDate(new Date(System.currentTimeMillis())));
                dataService.saveEntity(receiptToSign);
            }
        }
        if (signedReceipt != null) {
            try {
                xmlRequestReader.write(os, signedReceipt);
            } catch (WriteException e) {
                e.printStackTrace();
            }
        }
        return signedReceipt;
    }


    private boolean checkSignature(String publicKeyStr, String stringToVerify, String signatureToVerify) throws DataException {
        try {
            byte[] publicKeyBytes = decoder.decodeBuffer(publicKeyStr);
            byte[] signatureBytes = decoder.decodeBuffer(signatureToVerify);
            X509EncodedKeySpec x509KeySpec = new X509EncodedKeySpec(publicKeyBytes);
            KeyFactory keyFactory = KeyFactory.getInstance(KEY_ALGORITHM);
            PublicKey publicKey = keyFactory.generatePublic(x509KeySpec);
            Signature sig = Signature.getInstance(SIGNATURE_ALGORITHM);
            sig.initVerify(publicKey);
            sig.update(stringToVerify.getBytes());
            return sig.verify(signatureBytes);
        } catch (GeneralSecurityException e) {
            throw new DataException(Error.INTERNAL_ERROR);
        } catch (IOException e) {
            throw new DataException(Error.INTERNAL_ERROR);
        }
    }

    private ReceiptEntity signByHomeStore(AppstoreEntity appstore, String body) throws DataException {
        ReceiptEntity signedReceipt = null;
        try {
            String url = ApiMapping.SIGN_PURCHASE.getMethodUrl(appstore.getOpenaepUrl());
            URI uri = RequesterUtils.buildRequestUri(ApiMapping.SIGN_PURCHASE.getMethodUrl(appstore.getOpenaepUrl()),
                appstore.getAppstoreAccessToken(), null);
            HttpPost httpPost = new HttpPost(uri);
            httpPost.addHeader("authToken", appstore.getAppstoreAccessToken());
            httpPost.setEntity(new StringEntity(body));
            HttpResponse response = httpClient.execute(httpPost);

            int result = response.getStatusLine().getStatusCode();

            if (result == HttpStatus.SC_OK) {
                signedReceipt = (ReceiptEntity)xmlRequestReader.read(ReceiptEntity.class, response.getEntity().getContent());
            } else {
                throw new DataException(Error.INTERNAL_ERROR);
            }
        } catch (Exception e) {
            throw new DataException(Error.INTERNAL_ERROR);
        }
        return signedReceipt;
    }

}
