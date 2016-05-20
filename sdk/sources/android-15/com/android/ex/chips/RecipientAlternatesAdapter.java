/*
 * Copyright (C) 2011 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.android.ex.chips;

import com.android.ex.chips.BaseRecipientAdapter.EmailQuery;

import android.content.Context;
import android.database.Cursor;
import android.provider.ContactsContract.CommonDataKinds.Email;
import android.text.util.Rfc822Token;
import android.text.util.Rfc822Tokenizer;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CursorAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import java.util.HashMap;

public class RecipientAlternatesAdapter extends CursorAdapter {
    static final int MAX_LOOKUPS = 50;
    private final LayoutInflater mLayoutInflater;

    private final int mLayoutId;

    private final long mCurrentId;

    private int mCheckedItemPosition = -1;

    private OnCheckedItemChangedListener mCheckedItemChangedListener;

    private static final String TAG = "RecipAlternates";

    /**
     * Get a HashMap of address to RecipientEntry that contains all contact
     * information for a contact with the provided address, if one exists. This
     * may block the UI, so run it in an async task.
     *
     * @param context Context.
     * @param inAddresses Array of addresses on which to perform the lookup.
     * @return HashMap<String,RecipientEntry>
     */
    public static HashMap<String, RecipientEntry> getMatchingRecipients(Context context,
            String[] inAddresses) {
        int addressesSize = Math.min(MAX_LOOKUPS, inAddresses.length);
        String[] addresses = new String[addressesSize];
        StringBuilder bindString = new StringBuilder();
        // Create the "?" string and set up arguments.
        for (int i = 0; i < addressesSize; i++) {
            Rfc822Token[] tokens = Rfc822Tokenizer.tokenize(inAddresses[i].toLowerCase());
            addresses[i] = (tokens.length > 0 ? tokens[0].getAddress() : inAddresses[i]);
            bindString.append("?");
            if (i < addressesSize - 1) {
                bindString.append(",");
            }
        }

        if (Log.isLoggable(TAG, Log.DEBUG)) {
            Log.d(TAG, "Doing reverse lookup for " + addresses.toString());
        }

        HashMap<String, RecipientEntry> recipientEntries = new HashMap<String, RecipientEntry>();
        Cursor c = context.getContentResolver().query(Email.CONTENT_URI, EmailQuery.PROJECTION,
                Email.ADDRESS + " IN (" + bindString.toString() + ")", addresses, null);
        if (c != null) {
            try {
                if (c.moveToFirst()) {
                    do {
                        String address = c.getString(EmailQuery.ADDRESS);
                        recipientEntries.put(address, RecipientEntry.constructTopLevelEntry(
                                c.getString(EmailQuery.NAME),
                                c.getString(EmailQuery.ADDRESS),
                                c.getInt(EmailQuery.ADDRESS_TYPE),
                                c.getString(EmailQuery.ADDRESS_LABEL),
                                c.getLong(EmailQuery.CONTACT_ID),
                                c.getLong(EmailQuery.DATA_ID),
                                c.getString(EmailQuery.PHOTO_THUMBNAIL_URI)));
                        if (Log.isLoggable(TAG, Log.DEBUG)) {
                            Log.d(TAG, "Received reverse look up information for " + address
                                    + " RESULTS: "
                                    + " NAME : " + c.getString(EmailQuery.NAME)
                                    + " CONTACT ID : " + c.getLong(EmailQuery.CONTACT_ID)
                                    + " ADDRESS :" + c.getString(EmailQuery.ADDRESS));
                        }
                    } while (c.moveToNext());
                }
            } finally {
                c.close();
            }
        }
        return recipientEntries;
    }

    public RecipientAlternatesAdapter(Context context, long contactId, long currentId, int viewId,
            OnCheckedItemChangedListener listener) {
        super(context, context.getContentResolver().query(Email.CONTENT_URI, EmailQuery.PROJECTION,
                Email.CONTACT_ID + " =?", new String[] {
                    String.valueOf(contactId)
                }, null), 0);
        mLayoutInflater = LayoutInflater.from(context);
        mLayoutId = viewId;
        mCurrentId = currentId;
        mCheckedItemChangedListener = listener;
    }

    @Override
    public long getItemId(int position) {
        Cursor c = getCursor();
        if (c.moveToPosition(position)) {
            c.getLong(EmailQuery.DATA_ID);
        }
        return -1;
    }

    public RecipientEntry getRecipientEntry(int position) {
        Cursor c = getCursor();
        c.moveToPosition(position);
        return RecipientEntry.constructTopLevelEntry(c.getString(EmailQuery.NAME),
                c.getString(EmailQuery.ADDRESS), c.getInt(EmailQuery.ADDRESS_TYPE),
                c.getString(EmailQuery.ADDRESS_LABEL),
                c.getLong(EmailQuery.CONTACT_ID), c.getLong(EmailQuery.DATA_ID),
                c.getString(EmailQuery.PHOTO_THUMBNAIL_URI));
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        Cursor cursor = getCursor();
        cursor.moveToPosition(position);
        if (convertView == null) {
            convertView = newView();
        }
        if (cursor.getLong(EmailQuery.DATA_ID) == mCurrentId) {
            mCheckedItemPosition = position;
            if (mCheckedItemChangedListener != null) {
                mCheckedItemChangedListener.onCheckedItemChanged(mCheckedItemPosition);
            }
        }
        bindView(convertView, convertView.getContext(), cursor);
        return convertView;
    }

    // TODO: this is VERY similar to the BaseRecipientAdapter. Can we combine
    // somehow?
    @Override
    public void bindView(View view, Context context, Cursor cursor) {
        int position = cursor.getPosition();

        TextView display = (TextView) view.findViewById(android.R.id.title);
        ImageView imageView = (ImageView) view.findViewById(android.R.id.icon);
        RecipientEntry entry = getRecipientEntry(position);
        if (position == 0) {
            display.setText(cursor.getString(EmailQuery.NAME));
            display.setVisibility(View.VISIBLE);
            // TODO: see if this needs to be done outside the main thread
            // as it may be too slow to get immediately.
            imageView.setImageURI(entry.getPhotoThumbnailUri());
            imageView.setVisibility(View.VISIBLE);
        } else {
            display.setVisibility(View.GONE);
            imageView.setVisibility(View.GONE);
        }
        TextView destination = (TextView) view.findViewById(android.R.id.text1);
        destination.setText(cursor.getString(EmailQuery.ADDRESS));

        TextView destinationType = (TextView) view.findViewById(android.R.id.text2);
        if (destinationType != null) {
            destinationType.setText(Email.getTypeLabel(context.getResources(),
                    cursor.getInt(EmailQuery.ADDRESS_TYPE),
                    cursor.getString(EmailQuery.ADDRESS_LABEL)).toString().toUpperCase());
        }
    }

    @Override
    public View newView(Context context, Cursor cursor, ViewGroup parent) {
        return newView();
    }

    private View newView() {
        return mLayoutInflater.inflate(mLayoutId, null);
    }

    /*package*/ static interface OnCheckedItemChangedListener {
        public void onCheckedItemChanged(int position);
    }
}
