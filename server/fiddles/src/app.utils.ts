import glob from 'glob';
import fs from 'fs';
import path from 'path';

export const getFiles = (dir, results = []): string[] => {
  const dirents = fs.readdirSync(dir, { withFileTypes: true });

  for (const dirent of dirents) {
    const res = path.resolve(dir, dirent.name);
    if (dirent.isDirectory()) {
      getFiles(res, results);
    } else {
      results.push(res);
    }
  }
  return results;
};

export const getFileSize = (size) => {
  const fileSize = size.toString();
  if (fileSize.length < 4) return `${fileSize} bytes`;
  if (fileSize.length < 7)
    return `${Math.round(+fileSize / 1024).toFixed(2)} kb`;
  return `${(Math.round(+fileSize / 1024) / 1000).toFixed(2)} MB`;
};
