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

import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.os.Build;
import android.provider.ContactsContract;
import android.provider.ContactsContract.Data;
import android.text.TextUtils;
import android.widget.MultiAutoCompleteTextView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class ChipsUtil {

    /**
     * @return true when the caller can use Chips UI in its environment.
     */
    public static boolean supportsChipsUi() {
        return Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH;
    }

    public static boolean tryUpdateRecencyInfo(MultiAutoCompleteTextView... views) {
        for (MultiAutoCompleteTextView view : views) {
            if (view instanceof RecipientEditTextView) {
                updateRecencyInfo((RecipientEditTextView)view);
            }
        }
        return true;
    }

    // TODO: check this works
    public static void updateRecencyInfo(RecipientEditTextView view) {
        final Context context = view.getContext();
        final ContentResolver resolver = context.getContentResolver();
        final long currentTimeMillis = System.currentTimeMillis();

        final Collection<Long> contactIds = view.getContactIds();
        if (contactIds != null) {
            StringBuilder whereBuilder = new StringBuilder();
            ArrayList<String> whereArgs = new ArrayList<String>();
            String[] questionMarks = new String[contactIds.size()];
            for (Long contactId : contactIds) {
                whereArgs.add(String.valueOf(contactId));
            }
            Arrays.fill(questionMarks, "?");
            whereBuilder.append(ContactsContract.Contacts._ID + " IN (").
                    append(TextUtils.join(",", questionMarks)).
                    append(")");

            ContentValues values = new ContentValues();
            values.put(ContactsContract.Contacts.LAST_TIME_CONTACTED,
                    System.currentTimeMillis());
            resolver.update(ContactsContract.Contacts.CONTENT_URI, values,
                    whereBuilder.toString(), whereArgs.toArray(new String[0]));
        }

        final Collection<Long> dataIds = view.getDataIds();
        if (dataIds != null) {
            StringBuilder whereBuilder = new StringBuilder();
            ArrayList<String> whereArgs = new ArrayList<String>();
            String[] questionMarks = new String[dataIds.size()];
            for (Long dataId : dataIds) {
                whereArgs.add(String.valueOf(dataId));
            }
            Arrays.fill(questionMarks, "?");
            whereBuilder.append(ContactsContract.Data._ID + " IN (").
            append(TextUtils.join(",", questionMarks)).
            append(")");

            final ContentValues values = new ContentValues();
            values.put("last_time_contacted", currentTimeMillis);
            resolver.update(Data.CONTENT_URI, values,
                    whereBuilder.toString(), whereArgs.toArray(new String[0]));
        }
    }
}