// Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
//
// This file is part of the Hydra Ring Application.
//
// The Hydra Ring Application is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
//
// All names, logos, and references to "Deltares" are registered trademarks of
// Stichting Deltares and remain full property of Stichting Deltares at all times.
// All rights reserved.

// winapi.cpp : Defines the exported functions for the DLL application.
//

#include "windows.h"


struct typeVersionStruct {
    int versionMajor;
    int versionMinor;
    int versionRelease;
    int versionBuild;
};

extern "C" 
{
    /// <summary>
    /// Gets the file version.
    /// </summary>
    /// <param name="filename">The filename.</param>
    /// <param name="versionStruct">The version structure.</param>
    /// <param name="lengthFilename">The length of the filename string.</param>
    int GETFILEVERSIONC(char *filename, typeVersionStruct *versionStruct, int lengthFilename)
    {
        int returnCode = -1;
        LPCTSTR  lptstrFilename;
        lptstrFilename =  filename;
        DWORD dwDummy;
        DWORD dwLen = GetFileVersionInfoSize( lptstrFilename , &dwDummy); 
        LPBYTE lpVersionInfo = new BYTE[dwLen]; 
        BOOL result = GetFileVersionInfo(lptstrFilename, 0, dwLen, lpVersionInfo);
        if (result)
        {
            UINT uLen; 
            VS_FIXEDFILEINFO *lpFfi; 
            VerQueryValue( lpVersionInfo , "\\" , (LPVOID *)&lpFfi , &uLen ); 
            DWORD dwFileVersionMS = lpFfi->dwFileVersionMS; 
            DWORD dwFileVersionLS = lpFfi->dwFileVersionLS; 
            versionStruct->versionMajor = HIWORD(dwFileVersionMS);
            versionStruct->versionMinor = LOWORD(dwFileVersionMS);
            versionStruct->versionRelease = HIWORD(dwFileVersionLS);
            versionStruct->versionBuild = LOWORD(dwFileVersionLS);
            returnCode = 0;
        }
        delete[] lpVersionInfo;
        return returnCode;

    }
};

extern "C" 
{
    /// <summary>
    /// Gets the name of the module file.
    /// </summary>
    /// <param name="hModule">The h module.</param>
    /// <param name="filename">The filename.</param>
    /// <param name="lengthFilename">The length filename.</param>
    /// <returns></returns>
    int GETFILENAMEMODULEC (int *hModule, char *filename, int lengthFilename)
    {
        DWORD resultLength;
        resultLength = GetModuleFileName((HMODULE)*hModule, filename, lengthFilename); 
        return resultLength;
    }
};
